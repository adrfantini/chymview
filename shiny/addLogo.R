#From https://github.com/r-spatial/mapview/blob/master/R/addLogo.R
addLogo <- function (map, img, alpha = 1, src = c("remote", "local"), url,
    position = c("topleft", "topright", "bottomleft", "bottomright"),
    offset.x = 50, offset.y = 13, width = 60, height = 60)
{
    if (!missing(url))
        url <- paste0("\"", url, "\"")
    position <- position[1]
    src <- src[1]
    div_topleft <- paste0("newDiv.css({\n    'position': 'absolute',\n    'top': '",
        offset.y, "px',\n    'left': '", offset.x, "px',\n    'background-color': 'transparent',\n    'border': '0px solid black',\n    'width': '",
        width, "px',\n    'height': '", height, "px',\n  });")
    div_topright <- paste0("newDiv.css({\n    'position': 'absolute',\n    'top': '",
        offset.y, "px',\n    'right': '", offset.x, "px',\n    'background-color': 'transparent',\n    'border': '0px solid black',\n    'width': '",
        width, "px',\n    'height': '", height, "px',\n  });")
    div_bottomleft <- paste0("newDiv.css({\n    'position': 'absolute',\n    'bottom': '",
        offset.y, "px',\n    'left': '", offset.x, "px',\n    'background-color': 'transparent',\n    'border': '0px solid black',\n    'width': '",
        width, "px',\n    'height': '", height, "px',\n  });")
    div_bottomright <- paste0("newDiv.css({\n    'position': 'absolute',\n    'bottom': '",
        offset.y, "px',\n    'right': '", offset.x, "px',\n    'background-color': 'transparent',\n    'border': '0px solid black',\n    'width': '",
        width, "px',\n    'height': '", height, "px',\n  });")
    div <- switch(position, topleft = div_topleft, topright = div_topright,
        bottomleft = div_bottomleft, bottomright = div_bottomright)
    div_funk <- paste0("function(el, x, data) {\n                 // we need a new div element because we have to handle\n                 // the mouseover output seperately\n                 // debugger;\n                 function addElement () {\n                 // generate new div Element\n                 var newDiv = $(document.createElement('div'));\n                 // append at end of leaflet htmlwidget container\n                 $(el).append(newDiv);\n                 //provide ID and style\n                 newDiv.addClass('logo');\n",
        div, "return newDiv;\n                 }")
    div_add <- paste0("// check for already existing logo class to not duplicate\n                    var logo = $(el).find('.logo');\n                    if(!logo.length) {\n                      logo = addElement();")
    div_html <- switch(src, remote = remoteImage(img, alpha,
        url, width, height), local = localImage(img, alpha, url,
        width, height))
    render_stuff <- paste0(div_funk, div_add, div_html)
    map <- htmlwidgets::onRender(map, render_stuff)
    return(map)
}
