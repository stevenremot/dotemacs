const url = document.querySelector("[data-debug-url]").dataset.debugUrl;

function send(level, message) {
  return fetch(`${url}/log`, {
    method: "POST",
    headers: { "content-type": "application.json" },
    body: JSON.stringify({ level, message })
  });
}

function toString(payload) {
  if (payload instanceof Error) {
    return `${payload.toString()}
At ${payload.fileName}@${payload.columnNumber}:${payload.lineNumber}
${payload.stack}`;
  } else if (typeof payload === "string") {
    return payload;
  }

  return JSON.stringify(payload);
}

function makeSend(level) {
  return (...args) => send(level, args.map(toString).join(" "));
}

window.d = {
  log: makeSend("log"),
  info: makeSend("info"),
  warn: makeSend("warn"),
  error: makeSend("error")
};

window.addEventListener(
  "error",
  e => d.error("Uncaught error:", e.error),
  true
);

window.addEventListener(
  "unhandledrejection",
  e => d.error("Uncaught promise rejection:", e.reason),
  true
);
