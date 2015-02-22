$(function() {
    $(document).on("click", "a[data-method]", function() {
        var link = $(this);
        var method = link.data('method');
        var confirmText = link.attr("data-confirm");

        if (confirmText) {
            if (confirm(confirmText)) {
                handleMethod(link);
            }
        } else {
            handleMethod(link);
        }

        return false;
    });
});

function handleMethod(link) {
    var href = link.attr('href'),
        method = link.data('method'),
        csrfToken = $('meta[name=csrf-token]').attr('content'),
        form = $('<form method="POST" action="' + href + '?_method=' + method + '"></form>');

    if (csrfToken !== undefined) {
        var metadataInput = '<input name="_token" value="' + csrfToken + '" type="hidden" />';
        form.append(metadataInput);
    }

    form.appendTo('body');

    form.submit();
}
