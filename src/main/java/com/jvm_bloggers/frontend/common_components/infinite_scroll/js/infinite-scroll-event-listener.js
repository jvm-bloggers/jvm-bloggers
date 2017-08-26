$infiniteScrollContainer.on( 'load.infiniteScroll', function( event, response ) {
    var infScroll = $infiniteScrollContainer.data('infiniteScroll');
    var items = $(response).find("component")[0];
    if (items === undefined || response.indexOf('AccessDeniedPage') > -1) {
        infScroll.canLoad = false;
    }

    var $items = $(items).find('.item');
    $infiniteScrollContainer.infiniteScroll( 'appendItems', $items );
    infScroll.isLoading = false;

    if (infScroll.loadCount == $infiniteScrollTotalPageCount - 1) {
        $infiniteScrollContainer.infiniteScroll( 'option', {
            loadOnScroll: false
        });
    }
});