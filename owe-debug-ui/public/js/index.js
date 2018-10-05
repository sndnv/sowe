const endpoint = "localhost:9000";

$(window).on('load', function() {
    setup_entities_connection();
    setup_exchange_connection();
    setup_events_connection();
    init();
});

function init() {
    reset_construction_state();

    $('.entity-cell:not([data-entities=""])').each(function() {
        const cell = $(this);
        const entities = cell.data("entities");
        set_cell_class(cell, entities);
    });

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

    $(".entity-cell").contextmenu(function(element) {
        if(construction_state.active) {
            cancel_entity_construction();
        } else {
            const entity_refs = get_entity_refs_from_cell($(element.currentTarget));
            render_entities_details(entity_refs);
        }
        return false;
    }).click(function(element) {
        if(construction_state.active) {
            const cell = $(element.currentTarget);
            const cells = get_entity_cells(cell, construction_state.entity_size);
            const uri = "http://" + endpoint + "/entity";
            const csrf_token = $(".construction-info").attr("data-csrf-token");

            if(construction_state.create && cells.available) {
                const cell_point = extract_point_data(cell.attr("data-point"));

                $.ajax({
                    url: uri,
                    beforeSend: function(request) {
                        request.setRequestHeader("Csrf-Token", csrf_token);
                    },
                    type: 'POST',
                    data: {
                        x: cell_point.x,
                        y: cell_point.y,
                        entityType: construction_state.entity_type
                    }
                });
            } else if(!construction_state.create && !cells.available) {
                const entity_refs = get_entity_refs_from_cell(cell);
                $.ajax({
                    url: uri + "?entityId=" + encodeURIComponent(entity_refs[0]),
                    beforeSend: function(request) {
                        request.setRequestHeader("Csrf-Token", csrf_token);
                    },
                    type: 'DELETE'
                });
            }
        }

        return false;
    });

    $("#main-game-grid").delegate('td','mouseover mouseleave', function(e) {
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

    $(".construction-button").click(function(element) {
        cancel_entity_construction();
        const button = $(element.currentTarget);
        const construct = button.attr("data-construct");
        const construct_type = button.attr("data-construct-type")

        if(construct_type === "destroy") {
            enable_entity_destruction();
        } else if(construct_type != "") {
            enable_entity_construction(button, construct, construct_type);
        }
    });
}
