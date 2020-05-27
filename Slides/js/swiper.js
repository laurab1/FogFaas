var swiper = new Swiper('.swiper-container', {
  pagination: {
    el: '.swiper-pagination',
  },
});

var spans = document.getElementsByClassName("close");
var buttons = document.getElementsByClassName("button");
var modals = document.getElementsByClassName("w3-modal");

for(var i = 0; i < spans.length; i++) {
  buttons[i].onclick = function() {
    document.getElementById("modal" + this.id).style.display = "block";
  }

  spans[i].onclick = function() {
    document.getElementById("modal" + this.id).style.display = "none";
  }
}