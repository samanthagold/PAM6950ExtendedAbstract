library(circlize)

wd <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/"

xwalk <- read_csv(paste0(wd, "data/xwalk_regionsdivs.csv")) %>% 
    select(State, Region, Division)

migration <- read_csv(paste0(wd, "data/od_pooled.csv")) %>% 
    group_by(o_state_name, d_state_name) %>% 
    summarize(total_flows = sum(n))

circlize::chordDiagram(
    x = migration[, c("o_state_name", "d_state_name", "total_flows")],
    row.col = 1:51,
    directional = 1,
    direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow",
    annotationTrack = "grid",
    preAllocateTracks = 1
)
circlize::circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(
            mean(xlim),
            ylim[1] + .1,
            sector.name,
            facing = "clockwise",
            niceFacing = TRUE,
            adj = c(0, 0.5)
        )
        circos.axis(
            h = "top",
            labels.cex = 0.5,
            major.tick.length = 0.2,
            sector.index = sector.name,
            track.index = 2
        )
    },
    bg.border = NA
)

migration2 <- migration %>% 
    filter(o_state_name != d_state_name)

circlize::chordDiagram(
    x = migration2[, c("o_state_name", "d_state_name", "total_flows")],
    row.col = 1:51,
    directional = 1,
    direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow",
    annotationTrack = "grid",
    preAllocateTracks = 1
)

circlize::circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(
            mean(xlim),
            ylim[1] + .1,
            sector.name,
            facing = "clockwise",
            niceFacing = TRUE,
            adj = c(0, 0.5)
        )
        circos.axis(
            h = "top",
            labels.cex = 0.5,
            major.tick.length = 0.2,
            sector.index = sector.name,
            track.index = 2
        )
    },
    bg.border = NA
)

