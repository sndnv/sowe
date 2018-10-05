$(window).on('load', function() {
    init();
});

function init() {
    reset_construction_state();

    tippy(".entity-cell", {
        delay: 0,
        animateFill: false,
        arrow: true,
        followCursor: true,
        duration: [0, 0],
        placement: "right",
        html: "#cell-details",
        onShow(instance) {
            const content = this.querySelector('.tippy-content');
            const ref = $(instance.reference);
            const entities = JSON.parse(ref.attr("data-entities"));
            const point = extract_point_data(ref.attr("data-point"));
            content.innerHTML = render_entities_info(point, entities);
        }
    });

    tippy(".construction-button", {
        delay: 0,
        animateFill: false,
        arrow: true,
        followCursor: true,
        duration: [0, 0],
        placement: "right"
    });

    $("#generator-game-grid").delegate('td','mouseover mouseleave', function(e) {
        if(construction_state.active) {
            const target_cell = $(e.currentTarget);
            highlight_entity_cells(
                target_cell,
                construction_state.create ? construction_state.entity_size : {height: 1, width: 1}
            );
        }

        if (e.type == 'mouseover') {
            $(this).parent().addClass("grid-hover");
            $("colgroup").eq($(this).index()).addClass("grid-hover");
        } else {
            $(this).parent().removeClass("grid-hover");
            $("colgroup").eq($(this).index()).removeClass("grid-hover");
        }
    });

    const grid_parent = $("#main-game-grid").closest("li");
    const grid_width = grid_parent.width();
    grid_parent.siblings().width(grid_width);

    $("#generator-game-grid").selectable({
        filter: 'td'
    });

    var editor = ace.edit("generated-code");
    editor.getSession().setMode("ace/mode/scala");
    editor.setReadOnly(true);

    $(".construction-button").click(function(element) {
        const button = $(element.currentTarget);
        const construct = button.attr("data-construct");
        const construct_type = button.attr("data-construct-type")

        if(construct_type === "destroy") {
            clearCells(editor);
        } else if(construct_type != "") {
            generate_map_data(editor, get_cells_point_data(".ui-selected"), construct, construct_type);
        }
    });

    editor.setValue(generate_code());
}

function get_cells_point_data(selector) {
    return $(selector).map(function() {
      return extract_point_data($(this).attr("data-point"));
    }).get();
}

function clearCells(editor) {
    $.map($(".generator-cell"), function(elem) {
        const cell = $(elem);
        removeCellTypeClass(cell);
        cell.addClass("cell-type-land");
    });

    $('.ui-selected').removeClass('ui-selected')
    editor.setValue(generate_code());
}

function generate_map_data(editor, selected_cells, construct, construct_type) {
    if(selected_cells.length != 0) {
        $.map(selected_cells, function(cell_coords) {
           const cell = select_cell(cell_coords);
           removeCellTypeClass(cell);
           cell.addClass("cell-type-" + construct);
        });

        editor.setValue(generate_code());
    }
}

function removeCellTypeClass(cell) {
    cell.removeClass (function (index, className) {
        return (className.match (/(^|\s)cell-type-\S+/g) || []).join(' ');
    });
}

function generate_code() {
    const cells = get_cells_point_data(".generator-cell");

    const points = $.map(cells, function(cell_coords) {
        const cell = select_cell(cell_coords);
        const construct = getConstructFromCell(cell);

        if (construct != "land") {
            const point = "Point("+ cell_coords.x +", " +  cell_coords.y + ")";
            const cell_type = construct.charAt(0).toUpperCase() + construct.substr(1);
            return "   " + point + " -> Seq(UpdateType(Cell.Type." + cell_type + ")),";
        }
    });

    const result = ["val gridUpdates: Map[Point, Seq[Cell.Message]] = Map(" ].concat(points, [")", ""]).join("\n");

    return result;
}

function getConstructFromCell(cell) {
    const classes = $.map(cell[0].classList, function(class_name, i) {
      if (class_name.indexOf('cell-type-') === 0) {
        return class_name.replace('cell-type-', '');
      }
    });

    return classes[0];
}
