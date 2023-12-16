library(data.table)

# Read the csv
df <- setDT(read.csv('data/gdf_mit_walkdist_in_min.csv',
                 encoding='utf-8'))
df[, dist_unter_15km_travel := dist_km <= 1.5]
df[, n_dist_unter_15km_travel := sum(dist_unter_15km_travel), OBJECTID]
df[, sm_coordinates_comb := paste0("(", sup_lat, ", ", sup_long, ")")]
sort(table(df[n_sm_in_15 != 0, sm_coordinates_comb]), decreasing = TRUE)
df[n_sm_in_15 != 0, .(N = sum(Einwohner)), sm_coordinates_comb][order(-N)]

simple <- unique(df[, .(OBJECTID, Einwohner, Bundesland, Bezirk, n_sm_in_15, n_dist_unter_15km_travel)])
simple[, is_LandesHauptstadt := FALSE]
simple[Bezirk %in% c("Wien", "Sankt Pölten", "Graz", "Linz", "Salzburg", "Innsbruck", "Klagenfurt", "Bregenz", "Eisenstadt"), is_LandesHauptstadt := TRUE]
simple[, is_Vienna := FALSE]
simple[Bezirk %in% c("Wien"), is_Vienna := TRUE]
simple <- simple[Bundesland != ""]

# Einwohner pro Bezirk
simple[, Einwohner_Pro_Bezirk := sum(Einwohner), Bezirk]

# Einwohner pro Bundesland
simple[, Einwohner_Pro_Bundesland := sum(Einwohner), Bundesland]

sum(simple$Einwohner)
einw_pro_bl <- unique(simple[, .(Bundesland, Einwohner_Pro_Bundesland)])[order(-Einwohner_Pro_Bundesland)]
einw_pro_bez <- unique(simple[, .(Bezirk, Einwohner_Pro_Bezirk)])[order(-Einwohner_Pro_Bezirk)]



simple[, summe_luft := Einwohner * n_sm_in_15]
simple[is.na(n_dist_unter_15km_travel), n_dist_unter_15km_travel := 0]
simple[, summe_travel := Einwohner * n_dist_unter_15km_travel]

# Austria
sum(simple$Einwohner * simple$n_sm_in_15) / sum(simple$Einwohner)
sum(simple$Einwohner * simple$n_dist_unter_15km_travel) / sum(simple$Einwohner)

# By Bundesländer
simple[, bl_avg_air := sum(summe_luft) / sum(Einwohner), Bundesland]
bl_comp_air <- unique(simple[, .(Bundesland, bl_avg_air)])[order(-bl_avg_air)]
bl_comp_air

simple[, bl_avg_trav := sum(summe_travel) / sum(Einwohner), Bundesland]
bl_comp_trav <- unique(simple[, .(Bundesland, bl_avg_trav)])[order(-bl_avg_trav)]
bl_comp_trav

# By Bezirk
simple[, bez_avg_air := sum(summe_luft) / sum(Einwohner), Bezirk]
bez_comp_air <- unique(simple[, .(Bundesland, Bezirk, bez_avg_air)])[order(-bez_avg_air)]
bez_comp_air

simple[, bez_avg_trav := sum(summe_travel) / sum(Einwohner), Bezirk]
bez_comp_trav <- unique(simple[, .(Bundesland, Bezirk, bez_avg_trav)])[order(-bez_avg_trav)]
bez_comp_trav


# By Bundesland und Hauptsadt
simple[, blhs_avg_air := sum(summe_luft) / sum(Einwohner), c("Bundesland", "is_LandesHauptstadt")]
blhs_comp_air <- unique(simple[, .(Bundesland, is_LandesHauptstadt, blhs_avg_air)])[order(Bundesland, -blhs_avg_air)]
blhs_comp_air

simple[, blhs_avg_trav := sum(summe_travel) / sum(Einwohner), c("Bundesland", "is_LandesHauptstadt")]
blhs_comp_trav <- unique(simple[, .(Bundesland, is_LandesHauptstadt, blhs_avg_trav)])[order(Bundesland, -blhs_avg_trav)]
blhs_comp_trav

# By Landeshauptstadt
simple[, hs_avg_air := sum(summe_luft) / sum(Einwohner), is_LandesHauptstadt]
hs_comp_air <- unique(simple[, .(is_LandesHauptstadt, hs_avg_air)])[order(-hs_avg_air)]
hs_comp_air

simple[, hs_avg_trav := sum(summe_travel) / sum(Einwohner), is_LandesHauptstadt]
hs_comp_trav <- unique(simple[, .(is_LandesHauptstadt, hs_avg_trav)])[order(-hs_avg_trav)]
hs_comp_trav

# By Landeshauptstadt but without Vienna
simple[, hs_wow_avg_air := sum(summe_luft) / sum(Einwohner), c("is_Vienna", "is_LandesHauptstadt")]
hs_wow_comp_air <- unique(simple[, .(is_Vienna, is_LandesHauptstadt, hs_wow_avg_air)])[order(-hs_wow_avg_air)]
hs_wow_comp_air

simple[, hs_wow_avg_trav := sum(summe_travel) / sum(Einwohner), c("is_Vienna", "is_LandesHauptstadt")]
hs_wow_comp_trav <- unique(simple[, .(is_Vienna, is_LandesHauptstadt, hs_wow_avg_trav)])[order(-hs_wow_avg_trav)]
hs_wow_comp_trav

# Wien vs. nicht Wien
simple[, w_vs_nw_avg_air := sum(summe_luft) / sum(Einwohner), is_Vienna]
w_vs_nw_comp_air <- unique(simple[, .(is_Vienna, w_vs_nw_avg_air)])[order(is_Vienna, -w_vs_nw_avg_air)]
w_vs_nw_comp_air

simple[, w_vs_nw_avg_trav := sum(summe_travel) / sum(Einwohner), is_Vienna]
w_vs_nw_comp_trav <- unique(simple[, .(is_Vienna, w_vs_nw_avg_trav)])[order(is_Vienna, -w_vs_nw_avg_trav)]
w_vs_nw_comp_trav


# Einwohner pro Bezirk
simple[, Einwohner_Pro_Bezirk := sum(Einwohner), Bezirk]
temp <- unique(simple[, .(Bundesland, Bezirk, Einwohner_Pro_Bezirk)])

