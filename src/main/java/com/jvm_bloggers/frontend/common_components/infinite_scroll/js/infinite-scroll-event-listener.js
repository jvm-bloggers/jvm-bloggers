$infiniteScrollContainer.on( 'load.infiniteScroll', function( event, response ) {
    var items = $(response).find("component")[0];
    if (items === undefined || response.indexOf('AccessDeniedPage') > -1) {
        $infiniteScrollContainer.data('infiniteScroll').canLoad = false;
    }

    var $items = $(items).find('.item');
    $infiniteScrollContainer.infiniteScroll( 'appendItems', $items );
    $infiniteScrollContainer.data('infiniteScroll').isLoading = false;
});