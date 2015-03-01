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

function visualizeReadPages(pages, data) {
    var width = 400,
        barHeight = 20;

    var x = d3.scale.linear()
        .domain([0, pages])
        .range([0, width]);

    var chart = d3.select(".chart")
        .attr("width", width)
        .attr("height", barHeight);

    var bar = chart.selectAll("g")
        .data(data).enter()
        .append("rect")
        .attr("x", function(d, i) { return x(d[0]); })
        .attr("width", function(d, i) { return x(d[1] - d[0]); })
        .attr("height", barHeight);
}
