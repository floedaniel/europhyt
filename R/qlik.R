# Minimal client for the public TRACES plant-interceptions Qlik Sense app.
#
# The EC gateway only accepts engine websockets from a real browser session
# (with the page's qlik-csrf-token), so we load the public embed page in
# headless Chrome, reuse its websocket URL, and run Engine API calls from
# inside the page.

library(chromote)

QLIK_APP   <- "537e71fa-93ba-4ad7-85f7-6a00fa611e79"
QLIK_SHEET <- "b7eeb89e-8326-403b-9f23-ef88944b54ad"
QLIK_EMBED <- paste0(
  "https://dashboard.tech.ec.europa.eu/qs_digit_dashboard_mt/public/single/",
  "?appid=", QLIK_APP, "&sheet=", QLIK_SHEET, "&theme=sense&opt=ctxmenu,currsel"
)

qlik_connect <- function(timeout = 60) {
  b <- ChromoteSession$new()
  b$Network$enable()
  ws_url <- NULL
  b$Network$webSocketCreated(callback_ = function(x) {
    if (is.null(ws_url)) ws_url <<- x$url
  })
  b$Page$navigate(QLIK_EMBED, wait_ = FALSE)

  t0 <- Sys.time()
  while (is.null(ws_url) && Sys.time() - t0 < timeout) later::run_now(0.2)
  if (is.null(ws_url)) stop("Dashboard never opened an engine websocket")

  js <- sprintf('
    new Promise((resolve, reject) => {
      const s = new WebSocket(%s);
      let id = 0; const pend = {};
      s.onmessage = e => {
        const m = JSON.parse(e.data);
        if (m.id && pend[m.id]) { pend[m.id](m); delete pend[m.id]; }
      };
      s.onerror = () => reject("engine websocket error");
      s.onopen = () => {
        window.__rpc = (h, method, params) => new Promise(r => {
          id++; pend[id] = r;
          s.send(JSON.stringify({jsonrpc: "2.0", id, handle: h, method, params}));
        });
        resolve("ok");
      };
    })', jsonlite::toJSON(ws_url, auto_unbox = TRUE))
  b$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = timeout)

  doc <- qlik_rpc(b, -1, "OpenDoc", list(QLIK_APP))
  list(browser = b, doc = doc$qReturn$qHandle)
}

qlik_rpc <- function(b, handle, method, params = list()) {
  js <- sprintf(
    "window.__rpc(%d, %s, %s).then(m => JSON.stringify(m))",
    handle,
    jsonlite::toJSON(method, auto_unbox = TRUE),
    jsonlite::toJSON(params, auto_unbox = TRUE)
  )
  r <- b$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 300)
  m <- jsonlite::fromJSON(r$result$value, simplifyVector = FALSE)
  if (!is.null(m$error)) stop("Qlik engine: ", m$error$message)
  m$result
}

qlik_tables <- function(con) {
  res <- qlik_rpc(con$browser, con$doc, "GetTablesAndKeys", list(
    list(qcx = 1000, qcy = 1000), list(qcx = 0, qcy = 0), 30, TRUE, FALSE
  ))
  tibble::tibble(
    table  = vapply(res$qtr, `[[`, "", "qName"),
    n_rows = vapply(res$qtr, `[[`, 0, "qNoOfRows"),
    fields = lapply(res$qtr, \(t) vapply(t$qFields, `[[`, "", "qName")),
    is_key = lapply(res$qtr, \(t) vapply(t$qFields, \(f) f$qKeyType != "NOT_KEY", TRUE)),
    n_non_null = lapply(res$qtr, \(t) vapply(t$qFields, `[[`, 0, "qnNonNulls"))
  )
}

# Fetch a whole table as character columns (qText as published, NULL -> NA)
# via a hypercube with one dimension per field, so columns are labelled by
# construction. Key fields can pull in values from linked tables (e.g. a
# shared calendar), so NULLs are suppressed on a fully populated non-key field.
# A hypercube merges identical rows; the row count is checked by the caller.
qlik_table_data <- function(con, fields, is_key, n_non_null, n_rows) {
  anchor <- which(!is_key & n_non_null == n_rows)[1]
  dims <- lapply(seq_along(fields), \(k) list(
    qDef = list(qFieldDefs = list(fields[k])),
    qNullSuppression = isTRUE(k == anchor)
  ))
  obj <- qlik_rpc(con$browser, con$doc, "CreateSessionObject", list(list(
    qInfo = list(qType = "tabledump"),
    qHyperCubeDef = list(qDimensions = dims, qSuppressMissing = FALSE, qSuppressZero = FALSE)
  )))$qReturn
  size <- qlik_rpc(con$browser, obj$qHandle, "GetLayout")$qLayout$qHyperCube$qSize
  page <- floor(10000 / size$qcx)  # engine limit: 10 000 cells per page

  rows <- unlist(lapply(seq(0, max(size$qcy, 1) - 1, by = page), function(top) {
    res <- qlik_rpc(con$browser, obj$qHandle, "GetHyperCubeData", list(
      "/qHyperCubeDef", list(list(qTop = top, qLeft = 0, qHeight = page, qWidth = size$qcx))
    ))
    lapply(res[[1]][[1]]$qMatrix, \(r) vapply(r, \(v) if (isTRUE(v$qIsNull)) NA_character_ else v$qText %||% NA_character_, ""))
  }), recursive = FALSE)
  qlik_rpc(con$browser, con$doc, "DestroySessionObject", list(obj$qGenericId))

  m <- do.call(rbind, rows)
  colnames(m) <- fields
  tibble::as_tibble(m)
}
