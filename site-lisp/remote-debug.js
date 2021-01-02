const url = document.querySelector("[data-debug-url]").dataset.debugUrl;

function send(level, message) {
  return fetch(`${url}/log`, {
    method: "POST",
    headers: { "content-type": "application.json" },
    body: JSON.stringify({ level, message }),
  });
}

window.d = {
  log: (message) => send("log", message),
  error: (message) => send("error", message),
};

window.addEventListener("DOMContentLoaded", () =>
  window.addEventListener(
    "error",
    (e) => alert(e) || d.error(e.toString()),
    true
  )
);
