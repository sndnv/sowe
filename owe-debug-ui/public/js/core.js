var construction_state = {};

const endpoint = "localhost:9000";

$(window).on('load', function() {
    setup_entities_connection();
    setup_exchange_connection();
    setup_events_connection();
    init();
});

function setup_entities_connection() {
    const socket = new WebSocket("ws://" + endpoint + "/entities");

    socket.onmessage = function (e) {
        const event = JSON.parse(e.data);
        switch (event.id) {
            case "EntityCreated":
                add_entity(event.targetCell, event.mapEntity);
                break;
            case "EntityDestroyed":
                remove_entity(event.targetCell, event.mapEntity);
                break;
            case "EntityMoved":
                move_entity(event.targetCell, event.mapEntity);
                break;
            default:
                console.error("Unexpected event encountered: [" + event + "]");
        }
    };
}

function setup_exchange_connection() {
    const socket = new WebSocket("ws://" + endpoint + "/exchange");

    socket.onmessage = function (e) {
        const data = JSON.parse(e.data);
        render_exchange_stats(data.stats);
        render_exchange_commodities(data.commodities);
        render_exchange_entities(data.entities);
    };
}

function setup_events_connection() {
    const socket = new WebSocket("ws://" + endpoint + "/events");

    socket.onmessage = function (e) {
        const data = JSON.parse(e.data);
        render_event(data);
    };
}

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

    $(".game-grid").delegate('td','mouseover mouseleave', function(e) {
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

    const grid_parent = $(".game-grid").closest("li");
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

function highlight_entity_cells(cell, entity_size) {
    $("td").removeClass("cell-new-entity-available");
    $("td").removeClass("cell-new-entity-unavailable");

    const cells = get_entity_cells(cell, entity_size);

    $.each(cells.targets, function(i, cell) {
        cell.addClass(cells.available ? "cell-new-entity-available" : "cell-new-entity-unavailable")
    });
}

function get_entity_cells(cell, entity_size) {
    const cell_point = extract_point_data(cell.attr("data-point"));
    let target_cells = [];
    let cells_available = true;

    for (let x = cell_point.x; x < cell_point.x + entity_size.width; x++) {
        for (let y = cell_point.y; y < cell_point.y + entity_size.height; y++) {
            const cell = select_cell({x: x, y: y});
            target_cells.push(cell);
            if(!is_cell_available(cell)) {
                cells_available = false;
            }
        }
    }

    return {
        targets: target_cells,
        available: cells_available
    };
}

function is_cell_available(cell) {
    const entities = cell.attr("data-entities");
    if(entities != undefined && entities.length != 0) {
        return jQuery.isEmptyObject(JSON.parse(entities));
    } else {
        return false;
    }
}

function enable_entity_destruction() {
    disable_tooltips();
    const construction_info_container = $(".construction-info");
    construction_info_container.html(
        [
            "<h3 class='uk-card-title uk-text-muted'>Select entity to destroy...</h3>",
            "<button class='uk-button uk-button-default uk-width-1-1 cancel-destruction'>Cancel</button>"
        ].join("\n")
    );

    $(".cancel-destruction").click(cancel_entity_construction);

    set_construction_state_to_destroy();
}

function enable_entity_construction(button, construct, construct_type) {
    disable_tooltips();
    const construction_info_container = $(".construction-info");
    let construct_size = null;
    if(construct_type === "structure") {
        const structure_sizes = JSON.parse($(".structure-sizes").attr("data-sizes"));
        construct_size = structure_sizes[construct];
    } else {
        construct_size = {height: 1, width: 1};
    }

    $(".construction-button").removeClass("uk-button-primary");
    button.addClass("uk-button-primary");

    construction_info_container.html(
        [
            "<h3 class='uk-card-title uk-text-muted'> Select location for " + construct + "</h3>",
            "<p class='uk-text-left'><span class='uk-text-muted'>Properties:</span><div class='uk-text-left construct-properties'></div></p>",
            "<button class='uk-button uk-button-default uk-width-1-1 cancel-construction'>Cancel</button>"
        ].join("\n")
    );

    $(".construct-properties").jsonPresenter({json: construct_size});
    $(".cancel-construction").click(cancel_entity_construction);

    set_construction_state_to_create(construct_size, construct);
}

function disable_tooltips() {
    $.each($(".entity-cell"), function(i, cell) {
        cell._tippy.disable();
    });
}

function enable_tooltips() {
    $.each($(".entity-cell"), function(i, cell) {
        cell._tippy.enable();
    });
}

function set_construction_state_to_create(entity_size, entity_type) {
    construction_state = {
        active: true,
        create: true,
        entity_size: entity_size,
        entity_type: entity_type
    };
}

function set_construction_state_to_destroy() {
    construction_state = {
        active: true,
        create: false,
        entity_size: {height: 1, width: 1},
        entity_type: null
    };
}

function reset_construction_state() {
    construction_state = {
        active: false,
        create: true, // true == create; false == destroy;
        entity_size: {},
        entity_type: null
    };
}

function cancel_entity_construction() {
    enable_tooltips();
    $("td").removeClass("cell-new-entity-available");
    $("td").removeClass("cell-new-entity-unavailable");
    $(".construction-button").removeClass("uk-button-primary");
    const construction_info_container = $(".construction-info");
    construction_info_container.html("...");
    reset_construction_state();
}

function render_event(event) {
    const events_container = $(".game-events");

    const has_content = event.hasOwnProperty("mapEntity");

    const event_content = has_content ? "<div class='event-map-entity'></div>" : "<div>No additional data</div>";

    const event_cell = event.hasOwnProperty("targetCell")
        ? " @ <span class='uk-label uk-label-danger'>(" + event.targetCell.x + "," + event.targetCell.y + ")</span>"
        : "";

    events_container.prepend(
        [
            "<li>",
            "   <a class='uk-accordion-title uk-text-small' href='#'>" + event.id + event_cell + "</a>",
            "   <div class='uk-accordion-content'>",
            event_content,
            "   </div>",
            "</li>"
        ].join("\n")
    );

    if(has_content) {
        events_container.find(">:first-child .event-map-entity").jsonPresenter({json: event.mapEntity})
    }
}

function get_entity_refs_from_cell(cell) {
    return Object.keys(JSON.parse(cell.attr("data-entities")));
}

function render_entities_details(entity_refs) {
    const modal = $("#entities-details");
    const items_container = modal.find(".entities-details-items")
    const items_navigation = modal.find(".entities-details-items-dotnav")
    items_container.children().remove();
    items_navigation.children().remove();

    $.each(entity_refs, function (i, entity_id) {
        const uri = "http://" + endpoint + "/entity";
        $.get(uri, {entityId: entity_id}, function( data ) {
            items_container.append(
                [
                    "<li class='entity-details-" + i + "'>",
                    "   <div class='uk-text-meta uk-text-center uk-text-bold'>" + data.id + "</div>",
                    "   <div class='uk-child-width-1-3' uk-grid>",
                    "       <div class='uk-card-body entity-details-properties'>",
                    "           <span class='uk-text-muted'>Properties</span>",
                    "           <div class='content'></div>",
                    "       </div>",
                    "       <div class='uk-card-body entity-details-state'>",
                    "           <span class='uk-text-muted'>State</span>",
                    "           <div class='content'></div>",
                    "       </div>",
                    "       <div class='uk-card-body entity-details-modifiers'>",
                    "           <span class='uk-text-muted'>Modifiers</span>",
                    "           <div class='content'></div>",
                    "       </div>",
                    "   </div>",
                    "</li>"
                ].join("\n")
            );

            items_navigation.append(
                [
                    "<li uk-slideshow-item='" + i + "'>",
                    "<a href='#'></a>",
                    "</li>"
                ].join("\n")
            );

            $(".entity-details-" + i + " .entity-details-properties .content").jsonPresenter({json: data.properties});
            $(".entity-details-" + i + " .entity-details-state .content").jsonPresenter({json: data.state});
            $(".entity-details-" + i + " .entity-details-modifiers .content").jsonPresenter({json: data.modifiers});

            UIkit.modal(modal).show();
        });
    });
}

function add_entity(targetCell, entity) {
    const parentCell = select_cell(targetCell);
    const cells = get_entity_cells(parentCell, entity.spec.size);
    $.each(cells.targets, function(i, cell) {
        add_entity_data(cell, entity);
    });
}

function remove_entity(targetCell, entity) {
    const parentCell = select_cell(targetCell);
    const cells = get_entity_cells(parentCell, entity.spec.size);
    $.each(cells.targets, function(i, cell) {
        remove_entity_data(cell, entity);
    });
}

function move_entity(targetCell, entity) {
    const previous_cell = find_entity_cell(entity.entityRef);
    if(previous_cell.length != 0) {
        remove_entity_data(previous_cell, entity);
    }

    const cell = select_cell(targetCell);
    add_entity_data(cell, entity);
}

function add_entity_data(cell, entity) {
    const entities = JSON.parse(cell.attr("data-entities"));
    entities[entity.entityRef] = entity;
    cell.attr("data-entities", JSON.stringify(entities));
    set_cell_class(cell, entities);
}

function remove_entity_data(cell, entity) {
    const cell_entities = JSON.parse(cell.attr("data-entities"));
    delete cell_entities[entity.entityRef];
    cell.attr("data-entities", JSON.stringify(cell_entities));
    set_cell_class(cell, cell_entities);
}

function find_entity_cell(entity_ref) {
    return $(".entity-cell[data-entities*='" + entity_ref + "']");
}

function select_cell(targetCell) {
    return $("[data-point='(" + targetCell.x + "," + targetCell.y + ")']");
}

function set_cell_class(cell, entities) {
    const cell_class = $.map(entities, function (entity, ref) {
        return "cell-entity-" + entity_ref_to_type(ref).toLowerCase();
    });

    cell.removeClass (function (index, className) {
        return (className.match (/\bcell-entity-\S+/g) || []).join(' ');
    });

    cell.addClass(cell_class);
}

function extract_point_data(data) {
    const regex = /\((.+),(.+)\)/g;
    const result = regex.exec(data);
    return {
        x: parseInt(result[1]),
        y: parseInt(result[2])
    };
}

function entity_ref_to_type(entity_ref) {
    const regex = /^(.+)Ref\(/g;
    const result = regex.exec(entity_ref);
    return result[1];
}

function actor_id_from_ref(entity_ref) {
    const regex = /Actor\[akka:\/\/application\/user\/.*\/.+#(.+)\]/g;
    const result = regex.exec(entity_ref);
    return result[1];
}

function render_entities_info(point, entities) {
    const entities_count = Object.keys(entities).length ;

    const header = [
        "<div>",
        "   <span class='uk-label uk-label-success'>" + entities_count + "</span>",
        "   " + (entities_count === 1 ? "entity" : "entities") + " @ ",
        "   <span class='uk-label uk-label-danger'>(" + point.x + "," + point.y + ")</span>",
    ];

    const content = $.map(entities, function (entity, ref) {
        console.log(entity);
        return render_entity_info(entity);
    });

    const footer = [
        "</div>"
    ];

    return header.concat(content, footer).join("\n");
}

function render_entity_info(entity) {
    const html = [
        "<div class='uk-card uk-margin-small-top'>",
        "   <span class='uk-label uk-label-primary'>" + entity_ref_to_type(entity.entityRef) + "</span>",
        "   | ",
        "   <span class='uk-label uk-label-primary'>" + entity.spec.size.height + "x" + entity.spec.size.width + "</span>",
        "   @ ",
        "   <span class='uk-label uk-label-danger'>(" + entity.parentCell.x + "," + entity.parentCell.y + ")</span>",
        "   | ",
        "   <span class='uk-label uk-label-warning'>" + actor_id_from_ref(entity.entityRef) + "</span>",
        "</div>"
    ].join("\n");

    return html;
}

function render_exchange_stats(stats) {
    $(".exchange-stats").jsonPresenter({json: stats});
}

function render_exchange_commodities(commodities) {
    $(".exchange-commodities").jsonPresenter({json: commodities});
}

function render_exchange_entities(entities) {
    $(".exchange-entities").jsonPresenter({json: entities});
}
