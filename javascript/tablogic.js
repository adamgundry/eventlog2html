$(document).ready(function () {
    $("#closures-tab").on("shown.bs.tab", function (e) {
        $.sparkline_display_visible();
    });

    $("#profiteur-tab").on("shown.bs.tab", function (e) {
        var iFrame = document.getElementById( "profiteur-iframe" );
        if (iFrame) resizeIFrameToFitContent( iFrame );
    });
});

var resizeIFrameToFitContent = function( iFrame ) {
     // Set the iframe to be ~90% of the visible height.
    var domHeight = $(".container-fluid").height();
    iFrame.height = domHeight - (domHeight * 0.10);
};
