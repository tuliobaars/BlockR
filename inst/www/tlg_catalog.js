/*
 * TLG Catalog page: source-list selection, keyboard navigation and search.
 * Event delegation, so it works for pages rendered later too.
 */
(function () {
  "use strict";

  function select(item) {
    var list = item.closest(".tn-tlg-list");
    if (!list) return;
    var items = list.querySelectorAll(".tn-tlg-item");
    for (var i = 0; i < items.length; i++) {
      items[i].classList.remove("active");
      items[i].setAttribute("aria-selected", "false");
    }
    item.classList.add("active");
    item.setAttribute("aria-selected", "true");
    if (window.Shiny && window.Shiny.setInputValue && list.dataset.inputId) {
      window.Shiny.setInputValue(list.dataset.inputId, item.dataset.value);
    }
  }

  function visibleItems(list) {
    return Array.prototype.filter.call(list.querySelectorAll(".tn-tlg-item"), function (el) {
      return !el.hidden && !el.closest(".tn-tlg-group[hidden]");
    });
  }

  document.addEventListener("click", function (event) {
    var item = event.target.closest && event.target.closest(".tn-tlg-item");
    if (item) select(item);
  });

  // Up/down arrows move through the list (like a navigation drawer / source list)
  document.addEventListener("keydown", function (event) {
    var item = event.target.closest && event.target.closest(".tn-tlg-item");
    if (!item || (event.key !== "ArrowDown" && event.key !== "ArrowUp")) return;
    var items = visibleItems(item.closest(".tn-tlg-list"));
    var next = items[items.indexOf(item) + (event.key === "ArrowDown" ? 1 : -1)];
    if (next) {
      event.preventDefault();
      next.focus();
      select(next);
    }
  });

  // Search: by id, title or topic (every word must match); hide topics
  // without matches
  document.addEventListener("input", function (event) {
    var input = event.target;
    if (!input.classList || !input.classList.contains("tn-tlg-search")) return;
    var sidebar = input.closest(".tn-tlg-sidebar");
    var words = input.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
    var groups = sidebar.querySelectorAll(".tn-tlg-group");
    for (var g = 0; g < groups.length; g++) {
      var topic = (groups[g].getAttribute("data-topic") || "").toLowerCase();
      var items = groups[g].querySelectorAll(".tn-tlg-item");
      var any = false;
      for (var i = 0; i < items.length; i++) {
        var text = items[i].textContent.toLowerCase() + " " + topic;
        var match = words.every(function (w) { return text.indexOf(w) !== -1; });
        items[i].hidden = !match;
        any = any || match;
      }
      groups[g].hidden = !any;
    }
  });
})();
