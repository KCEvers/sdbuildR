// Keep bslib navset_* pill highlighting correct under pkgdown's Bootstrap 5.
//
// bslib emits shiny-legacy tab markup where the *selected* item is marked with
// <li class="active">. Bootstrap 5's tab plugin, however, toggles .active on the
// <a> and never touches the <li>, so the initially-selected pill's <li class=
// "active"> is never cleared -- leaving two pills highlighted after a click.
// This delegated handler restores the Bootstrap 3-style behaviour bslib relies
// on: on click, clear .active from sibling <li> elements and set it on the
// clicked pill's <li>.
document.addEventListener("click", function (e) {
  var a = e.target.closest('a[data-bs-toggle="tab"], a[data-toggle="tab"]');
  if (!a) return;
  var ul = a.closest("ul.nav-pills, ul.nav-tabs");
  if (!ul) return;
  ul.querySelectorAll(":scope > li.active").forEach(function (li) {
    li.classList.remove("active");
  });
  if (a.parentElement && a.parentElement.matches("li")) {
    a.parentElement.classList.add("active");
  }
});
