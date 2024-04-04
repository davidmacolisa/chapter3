#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(stringr)
library(statar)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
sec <- read_rds(file = "./Data_PhD/US/SEC/sec.rds") %>%
  filter(!stprba %in% c("AK", "HI", "DC", ""))
# sec.BS <- filter(sec, stmt == "BS") #firm balance sheet data
#======================================================================================================================#
### Getting the Sectors, Columns and Merging with TRI.QCEW Data
#======================================================================================================================#
### Manufacturing
#======================================================================================================================#
sort(unique(sec$year))
sort(unique(sec$sic))
sort(unique(sec$stprba))

# sec.manu <- sec %>% filter(str_detect(sic, pattern = "^2") | str_detect(sic, pattern = "^3"))#manufacturing
# sort(unique(sec.manu$sic))
#======================================================================================================================#
### Assets
#======================================================================================================================#
sec.Assets <- sec %>%
  filter(
    tlabel %in% c("Assets", "Total Assets") &
      datatype %in% "monetary" &
      grepl(pattern = "Q", x = fp)
  ) %>%
  # filter(
  #   grepl(pattern = "^2", x = sic) | #manufacturing
  #     grepl(pattern = "^3", x = sic)
  # ) %>%
  rename(assets = value, assets_label = tlabel, assets_doc = doc) %>%
  mutate(assets = as.numeric(assets)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, year, assets) %>%
  data.frame()

sort(unique(sec.Assets$stprba))
sort(unique(sec.Assets$year))
sort(unique(sec.Assets$assets_label))
sort(unique(sec.Assets$fp))
n_distinct(sec.Assets$name)

library(collapse) # for collapsing dataframe
sec.Assets <- collap(
  sec.Assets,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)

sort(unique(sec.Assets$stprba))
sort(unique(sec.Assets$name))
sec.Assets <- sec.Assets[complete.cases(sec.Assets),]

#======================================================================================================================#
### Inventory
#======================================================================================================================#
sec.Inventory <- sec %>%
  filter(
    tlabel %in% c("Inventory", "Inventory, Net") &
      grepl(pattern = "monetary", x = datatype) &
      grepl(pattern = "Q", x = fp)
  ) %>%
  rename(inventory = value, inventory_label = tlabel, inventory_doc = doc) %>%
  mutate(inventory = as.numeric(inventory)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, year, inventory)

sort(unique(sec.Inventory$stprba))
sort(unique(sec.Inventory$year))
sort(unique(sec.Inventory$inventory_label))
sort(unique(sec.Inventory$fp))
n_distinct(sec.Inventory$name)

sec.Inventory <- collap(
  sec.Inventory,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)
sec.Inventory <- sec.Inventory[complete.cases(sec.Inventory),]

#======================================================================================================================#
### Revenue
#======================================================================================================================#
sec.Revenue <- sec %>%
  filter(
    tlabel %in% c("Revenue", "Revenue, Net", "Revenues", "Revenues, Net") &
      grepl(pattern = "monetary", x = datatype) &
      grepl(pattern = "Q", x = fp)
  ) %>%
  rename(revenue = value, revenue_label = tlabel, revenue_doc = doc) %>%
  mutate(revenue = as.numeric(revenue)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, revenue, year)

sort(unique(sec.Revenue$stprba))
sort(unique(sec.Revenue$revenue_label))
sort(unique(sec.Revenue$year))
length(unique(sec.Revenue$name))

sec.Revenue <- collap(
  sec.Revenue,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)

sort(unique(sec.Revenue$stprba))
sort(unique(sec.Revenue$name))
sec.Revenue <- sec.Revenue[complete.cases(sec.Revenue),]

#======================================================================================================================#
### Cost
#======================================================================================================================#
sec.OperatingCost <- sec %>%
  filter(
    tlabel %in% c("Cost and Expense", "Operating Cost and Expense",
                  "Costs and Expenses", "Operating Costs and Expenses") &
      grepl(pattern = "monetary", x = datatype) &
      grepl(pattern = "Q", x = fp)
  ) %>%
  rename(operating_cost = value, operating_cost_label = tlabel, operating_cost_doc = doc) %>%
  mutate(operating_cost = as.numeric(operating_cost)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, operating_cost, year)

sec.OperatingCost <- collap(
  sec.OperatingCost,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)

sort(unique(sec.OperatingCost$stprba))
sort(unique(sec.OperatingCost$name))
sec.OperatingCost <- sec.OperatingCost[complete.cases(sec.OperatingCost),]

sec.CostGoods <- sec %>%
  filter(
    tlabel %in% "Cost of Revenue" &
      grepl(pattern = "monetary", x = datatype) &
      grepl(pattern = "Q", x = fp)
  ) %>%
  rename(cost_goods = value, cost_goods_label = tlabel, cost_goods_doc = doc) %>%
  mutate(cost_goods = as.numeric(cost_goods)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, cost_goods, year)

sec.CostGoods <- collap(
  sec.CostGoods,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)

sort(unique(sec.CostGoods$stprba))
sort(unique(sec.CostGoods$name))
sec.CostGoods <- sec.CostGoods[complete.cases(sec.CostGoods),]

sec.LabourCost <- sec %>%
  filter(
    tlabel %in% "Labor and Related Expense" &
      grepl(pattern = "monetary", x = datatype) &
      grepl(pattern = "Q", x = fp)
  ) %>%
  rename(labour_cost = value, labour_cost_label = tlabel, labour_cost_doc = doc) %>%
  mutate(labour_cost = as.numeric(labour_cost)) %>%
  select(adsh, name, stprba, cityba, zipba, sic, labour_cost, year)

sort(unique(sec.LabourCost$stprba))
sort(unique(sec.LabourCost$year))
sort(unique(sec.LabourCost$name))
length(unique(sec.LabourCost$name))

sec.LabourCost <- collap(
  sec.LabourCost,
  ~name + year,
  na.rm = T,
  FUN = fmean,
  keep.col.order = T,
  sort = T,
  decreasing = F,
)

sort(unique(sec.LabourCost$stprba))
sort(unique(sec.LabourCost$name))
sec.LabourCost <- sec.LabourCost[complete.cases(sec.LabourCost),]
#======================================================================================================================#
### Merging the data
#======================================================================================================================#
sec.new <- sec.Assets %>%
  left_join(
    y = sec.Inventory,
    by = c("adsh" = "adsh", "stprba" = "stprba", "name" = "name", "cityba" = "cityba", "zipba" = "zipba", "sic" = "sic", "year" = "year")
  ) %>%
  left_join(
    y = sec.Revenue,
    by = c("adsh" = "adsh", "stprba" = "stprba", "name" = "name", "cityba" = "cityba", "zipba" = "zipba", "sic" = "sic", "year" = "year")
  ) %>%
  left_join(
    y = sec.OperatingCost,
    by = c("adsh" = "adsh", "stprba" = "stprba", "name" = "name", "cityba" = "cityba", "zipba" = "zipba", "sic" = "sic", "year" = "year")
  ) %>%
  left_join(
    y = sec.CostGoods,
    by = c("adsh" = "adsh", "stprba" = "stprba", "name" = "name", "cityba" = "cityba", "zipba" = "zipba", "sic" = "sic", "year" = "year")
  ) %>%
  left_join(
    y = sec.LabourCost,
    by = c("adsh" = "adsh", "stprba" = "stprba", "name" = "name", "cityba" = "cityba", "zipba" = "zipba", "sic" = "sic", "year" = "year")
  ) %>%
  data.frame()

# sec.new <- sec.new[complete.cases(sec.new),]
# write_rds(sec.new, file = "./Data_PhD/US/SEC/sec_new.rds", compress = "xz")
# sort(unique(sec.new$name))
