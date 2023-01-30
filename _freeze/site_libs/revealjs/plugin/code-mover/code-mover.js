window.CodeMover = window.CodeMover || {
  id: 'CodeMover',
  init: function(deck) {
      initCodeMover(deck);
  }
};

const initCodeMover = function(Reveal){

window.addEventListener( 'ready', function( event ) {
  const codeTabs = document.getElementsByClassName("move-code");
  for(const codeTab of codeTabs) {
    const cellCode = codeTab.querySelector(".cell > .cell-code");
    const parentPanel = codeTab.closest(".tab-content");
    const nextTab = parentPanel.children[1];
    nextTab.appendChild(cellCode);
  }

});
};
