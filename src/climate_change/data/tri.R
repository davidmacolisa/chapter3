#======================================================#
### API AND DATA
#======================================================#
## Packages
#======================================================#
library(ndjson)
library(dplyr)
library(httr)
library(tidyr)
library(readr)
library(statar)
#======================================================#
## Working Directory
#======================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#=========================================#
### TOXIC RELEASE INVENTORY - TRI
#=========================================#
library(data.table)

### YEAR 2022
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022"
)


# Merging documents
filepath.us.1a.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_1a_2022.txt"
filepath.us.1b.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_1b_2022.txt"
filepath.us.2a.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_2a_2022.txt"
filepath.us.2b.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_2b_2022.txt"
filepath.us.3a.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_3a_2022.txt"
filepath.us.3b.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_3b_2022.txt"
filepath.us.3c.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_3c_2022.txt"
filepath.us.4.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_4_2022.txt"
filepath.us.5.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_5_2022.txt"
filepath.us.6.2022 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2022/US_6_2022.txt"

# facility chemical releases and other waste management
us.1a.2022 <- fread(file = filepath.us.1a.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2022
us.1b.2022 <- fread(file = filepath.us.1b.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2022 <- merge(x = us.1a.2022, y = us.1b.2022,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2022 <- fread(file = filepath.us.2a.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2022 <- merge(x = tri2022, y = us.2a.2022,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2022 <- fread(file = filepath.us.2b.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2022 <- merge(x = tri2022, y = us.2b.2022,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2022 <- fread(file = filepath.us.3a.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2022 <- merge(x = tri2022, y = us.3a.2022,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2022 <- fread(file = filepath.us.3b.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2022 <- fread(file = filepath.us.3c.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2022 <- merge(x = tri2022, y = us.3c.2022,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2022 <- fread(file = filepath.us.4.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2022 <- merge(x = tri2022, y = us.4.2022,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2022 <- fread(file = filepath.us.5.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2022 <- merge(x = tri2022, y = us.5.2022,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# miscelleneous and optional comments
# us.6.2022 <- fread(file = filepath.us.6.2022, header = T, fill = T, check.names = T, sep = "\t", quote = "")

tri2022[] <- lapply(tri2022[], as.factor) # converting to factor


### YEAR 2021
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021"
)

# Merging documents
filepath.us.1a.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_1a_2021.txt"
filepath.us.1b.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_1b_2021.txt"
filepath.us.2a.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_2a_2021.txt"
filepath.us.2b.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_2b_2021.txt"
filepath.us.3a.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_3a_2021.txt"
filepath.us.3b.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_3b_2021.txt"
filepath.us.3c.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_3c_2021.txt"
filepath.us.4.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_4_2021.txt"
filepath.us.5.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_5_2021.txt"
filepath.us.6.2021 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2021/US_6_2021.txt"


# facility chemical releases and other waste management
us.1a.2021 <- fread(file = filepath.us.1a.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2021
us.1b.2021 <- fread(file = filepath.us.1b.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2021 <- merge(x = us.1a.2021, y = us.1b.2021,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2021 <- fread(file = filepath.us.2a.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2021 <- merge(x = tri2021, y = us.2a.2021,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2021 <- fread(file = filepath.us.2b.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2021 <- merge(x = tri2021, y = us.2b.2021,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2021 <- fread(file = filepath.us.3a.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2021 <- merge(x = tri2021, y = us.3a.2021,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2021 <- fread(file = filepath.us.3b.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2021 <- fread(file = filepath.us.3c.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2021 <- merge(x = tri2021, y = us.3c.2021,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2021 <- fread(file = filepath.us.4.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2021 <- merge(x = tri2021, y = us.4.2021,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2021 <- fread(file = filepath.us.5.2021, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2021 <- merge(x = tri2021, y = us.5.2021,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2021[] <- lapply(tri2021[], as.factor) # converting to factor


### YEAR 2020
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020"
)

# Merging documents
filepath.us.1a.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_1a_2020.txt"
filepath.us.1b.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_1b_2020.txt"
filepath.us.2a.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_2a_2020.txt"
filepath.us.2b.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_2b_2020.txt"
filepath.us.3a.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_3a_2020.txt"
filepath.us.3b.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_3b_2020.txt"
filepath.us.3c.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_3c_2020.txt"
filepath.us.4.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_4_2020.txt"
filepath.us.5.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_5_2020.txt"
filepath.us.6.2020 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2020/US_6_2020.txt"


# facility chemical releases and other waste management
us.1a.2020 <- fread(file = filepath.us.1a.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2020
us.1b.2020 <- fread(file = filepath.us.1b.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2020 <- merge(x = us.1a.2020, y = us.1b.2020,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2020 <- fread(file = filepath.us.2a.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2020 <- merge(x = tri2020, y = us.2a.2020,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2020 <- fread(file = filepath.us.2b.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2020 <- merge(x = tri2020, y = us.2b.2020,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2020 <- fread(file = filepath.us.3a.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2020 <- merge(x = tri2020, y = us.3a.2020,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2020 <- fread(file = filepath.us.3b.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2020 <- fread(file = filepath.us.3c.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2020 <- merge(x = tri2020, y = us.3c.2020,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2020 <- fread(file = filepath.us.4.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2020 <- merge(x = tri2020, y = us.4.2020,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2020 <- fread(file = filepath.us.5.2020, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2020 <- merge(x = tri2020, y = us.5.2020,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2020[] <- lapply(tri2020[], as.factor) # converting to factor


### YEAR 2019
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019"
)

# Merging documents
filepath.us.1a.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_1a_2019.txt"
filepath.us.1b.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_1b_2019.txt"
filepath.us.2a.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_2a_2019.txt"
filepath.us.2b.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_2b_2019.txt"
filepath.us.3a.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_3a_2019.txt"
filepath.us.3b.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_3b_2019.txt"
filepath.us.3c.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_3c_2019.txt"
filepath.us.4.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_4_2019.txt"
filepath.us.5.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_5_2019.txt"
filepath.us.6.2019 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2019/US_6_2019.txt"


# facility chemical releases and other waste management
us.1a.2019 <- fread(file = filepath.us.1a.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2019
us.1b.2019 <- fread(file = filepath.us.1b.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2019 <- merge(x = us.1a.2019, y = us.1b.2019,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2019 <- fread(file = filepath.us.2a.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2019 <- merge(x = tri2019, y = us.2a.2019,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2019 <- fread(file = filepath.us.2b.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2019 <- merge(x = tri2019, y = us.2b.2019,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2019 <- fread(file = filepath.us.3a.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2019 <- merge(x = tri2019, y = us.3a.2019,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2019 <- fread(file = filepath.us.3b.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2019 <- fread(file = filepath.us.3c.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2019 <- merge(x = tri2019, y = us.3c.2019,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2019 <- fread(file = filepath.us.4.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2019 <- merge(x = tri2019, y = us.4.2019,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2019 <- fread(file = filepath.us.5.2019, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2019 <- merge(x = tri2019, y = us.5.2019,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2019[] <- lapply(tri2019[], as.factor) # converting to factor


### YEAR 2018
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018"
)

# Merging documents
filepath.us.1a.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_1a_2018.txt"
filepath.us.1b.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_1b_2018.txt"
filepath.us.2a.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_2a_2018.txt"
filepath.us.2b.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_2b_2018.txt"
filepath.us.3a.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_3a_2018.txt"
filepath.us.3b.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_3b_2018.txt"
filepath.us.3c.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_3c_2018.txt"
filepath.us.4.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_4_2018.txt"
filepath.us.5.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_5_2018.txt"
filepath.us.6.2018 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2018/US_6_2018.txt"


# facility chemical releases and other waste management
us.1a.2018 <- fread(file = filepath.us.1a.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2018
us.1b.2018 <- fread(file = filepath.us.1b.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2018 <- merge(x = us.1a.2018, y = us.1b.2018,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2018 <- fread(file = filepath.us.2a.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2018 <- merge(x = tri2018, y = us.2a.2018,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2018 <- fread(file = filepath.us.2b.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2018 <- merge(x = tri2018, y = us.2b.2018,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2018 <- fread(file = filepath.us.3a.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2018 <- merge(x = tri2018, y = us.3a.2018,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2018 <- fread(file = filepath.us.3b.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2018 <- fread(file = filepath.us.3c.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2018 <- merge(x = tri2018, y = us.3c.2018,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2018 <- fread(file = filepath.us.4.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2018 <- merge(x = tri2018, y = us.4.2018,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2018 <- fread(file = filepath.us.5.2018, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2018 <- merge(x = tri2018, y = us.5.2018,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2018[] <- lapply(tri2018[], as.factor) # converting to factor


### YEAR 2017
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017"
)

# Merging documents
filepath.us.1a.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_1a_2017.txt"
filepath.us.1b.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_1b_2017.txt"
filepath.us.2a.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_2a_2017.txt"
filepath.us.2b.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_2b_2017.txt"
filepath.us.3a.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_3a_2017.txt"
filepath.us.3b.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_3b_2017.txt"
filepath.us.3c.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_3c_2017.txt"
filepath.us.4.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_4_2017.txt"
filepath.us.5.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_5_2017.txt"
filepath.us.6.2017 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2017/US_6_2017.txt"


# facility chemical releases and other waste management
us.1a.2017 <- fread(file = filepath.us.1a.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2017
us.1b.2017 <- fread(file = filepath.us.1b.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2017 <- merge(x = us.1a.2017, y = us.1b.2017,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2017 <- fread(file = filepath.us.2a.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2017 <- merge(x = tri2017, y = us.2a.2017,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2017 <- fread(file = filepath.us.2b.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2017 <- merge(x = tri2017, y = us.2b.2017,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2017 <- fread(file = filepath.us.3a.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2017 <- merge(x = tri2017, y = us.3a.2017,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2017 <- fread(file = filepath.us.3b.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2017 <- fread(file = filepath.us.3c.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2017 <- merge(x = tri2017, y = us.3c.2017,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2017 <- fread(file = filepath.us.4.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2017 <- merge(x = tri2017, y = us.4.2017,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2017 <- fread(file = filepath.us.5.2017, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2017 <- merge(x = tri2017, y = us.5.2017,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2017[] <- lapply(tri2017[], as.factor) # converting to factor


### YEAR 2016
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016"
)

# Merging documents
filepath.us.1a.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_1a_2016.txt"
filepath.us.1b.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_1b_2016.txt"
filepath.us.2a.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_2a_2016.txt"
filepath.us.2b.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_2b_2016.txt"
filepath.us.3a.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_3a_2016.txt"
filepath.us.3b.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_3b_2016.txt"
filepath.us.3c.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_3c_2016.txt"
filepath.us.4.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_4_2016.txt"
filepath.us.5.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_5_2016.txt"
filepath.us.6.2016 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2016/US_6_2016.txt"


# facility chemical releases and other waste management
us.1a.2016 <- fread(file = filepath.us.1a.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2016
us.1b.2016 <- fread(file = filepath.us.1b.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2016 <- merge(x = us.1a.2016, y = us.1b.2016,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2016 <- fread(file = filepath.us.2a.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2016 <- merge(x = tri2016, y = us.2a.2016,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2016 <- fread(file = filepath.us.2b.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2016 <- merge(x = tri2016, y = us.2b.2016,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2016 <- fread(file = filepath.us.3a.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2016 <- merge(x = tri2016, y = us.3a.2016,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2016 <- fread(file = filepath.us.3b.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2016 <- fread(file = filepath.us.3c.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2016 <- merge(x = tri2016, y = us.3c.2016,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2016 <- fread(file = filepath.us.4.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2016 <- merge(x = tri2016, y = us.4.2016,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2016 <- fread(file = filepath.us.5.2016, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2016 <- merge(x = tri2016, y = us.5.2016,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2016[] <- lapply(tri2016[], as.factor) # converting to factor


### YEAR 2015
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015"
)

# Merging documents
filepath.us.1a.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_1a_2015.txt"
filepath.us.1b.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_1b_2015.txt"
filepath.us.2a.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_2a_2015.txt"
filepath.us.2b.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_2b_2015.txt"
filepath.us.3a.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_3a_2015.txt"
filepath.us.3b.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_3b_2015.txt"
filepath.us.3c.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_3c_2015.txt"
filepath.us.4.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_4_2015.txt"
filepath.us.5.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_5_2015.txt"
filepath.us.6.2015 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2015/US_6_2015.txt"


# facility chemical releases and other waste management
us.1a.2015 <- fread(file = filepath.us.1a.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2015
us.1b.2015 <- fread(file = filepath.us.1b.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2015 <- merge(x = us.1a.2015, y = us.1b.2015,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2015 <- fread(file = filepath.us.2a.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2015 <- merge(x = tri2015, y = us.2a.2015,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2015 <- fread(file = filepath.us.2b.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2015 <- merge(x = tri2015, y = us.2b.2015,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2015 <- fread(file = filepath.us.3a.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2015 <- merge(x = tri2015, y = us.3a.2015,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2015 <- fread(file = filepath.us.3b.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2015 <- fread(file = filepath.us.3c.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2015 <- merge(x = tri2015, y = us.3c.2015,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2015 <- fread(file = filepath.us.4.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2015 <- merge(x = tri2015, y = us.4.2015,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2015 <- fread(file = filepath.us.5.2015, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2015 <- merge(x = tri2015, y = us.5.2015,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2015[] <- lapply(tri2015[], as.factor) # converting to factor


### YEAR 2014
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014"
)

# Merging documents
filepath.us.1a.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_1a_2014.txt"
filepath.us.1b.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_1b_2014.txt"
filepath.us.2a.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_2a_2014.txt"
filepath.us.2b.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_2b_2014.txt"
filepath.us.3a.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_3a_2014.txt"
filepath.us.3b.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_3b_2014.txt"
filepath.us.3c.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_3c_2014.txt"
filepath.us.4.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_4_2014.txt"
filepath.us.5.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_5_2014.txt"
filepath.us.6.2014 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2014/US_6_2014.txt"


# facility chemical releases and other waste management
us.1a.2014 <- fread(file = filepath.us.1a.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2014
us.1b.2014 <- fread(file = filepath.us.1b.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2014 <- merge(x = us.1a.2014, y = us.1b.2014,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2014 <- fread(file = filepath.us.2a.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2014 <- merge(x = tri2014, y = us.2a.2014,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2014 <- fread(file = filepath.us.2b.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2014 <- merge(x = tri2014, y = us.2b.2014,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2014 <- fread(file = filepath.us.3a.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2014 <- merge(x = tri2014, y = us.3a.2014,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2014 <- fread(file = filepath.us.3b.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2014 <- fread(file = filepath.us.3c.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2014 <- merge(x = tri2014, y = us.3c.2014,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2014 <- fread(file = filepath.us.4.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2014 <- merge(x = tri2014, y = us.4.2014,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2014 <- fread(file = filepath.us.5.2014, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2014 <- merge(x = tri2014, y = us.5.2014,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2014[] <- lapply(tri2014[], as.factor) # converting to factor


### YEAR 2013
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013"
)

# Merging documents
filepath.us.1a.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_1a_2013.txt"
filepath.us.1b.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_1b_2013.txt"
filepath.us.2a.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_2a_2013.txt"
filepath.us.2b.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_2b_2013.txt"
filepath.us.3a.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_3a_2013.txt"
filepath.us.3b.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_3b_2013.txt"
filepath.us.3c.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_3c_2013.txt"
filepath.us.4.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_4_2013.txt"
filepath.us.5.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_5_2013.txt"
filepath.us.6.2013 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2013/US_6_2013.txt"


# facility chemical releases and other waste management
us.1a.2013 <- fread(file = filepath.us.1a.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2013
us.1b.2013 <- fread(file = filepath.us.1b.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2013 <- merge(x = us.1a.2013, y = us.1b.2013,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2013 <- fread(file = filepath.us.2a.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2013 <- merge(x = tri2013, y = us.2a.2013,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2013 <- fread(file = filepath.us.2b.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2013 <- merge(x = tri2013, y = us.2b.2013,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2013 <- fread(file = filepath.us.3a.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2013 <- merge(x = tri2013, y = us.3a.2013,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2013 <- fread(file = filepath.us.3b.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2013 <- fread(file = filepath.us.3c.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2013 <- merge(x = tri2013, y = us.3c.2013,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2013 <- fread(file = filepath.us.4.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2013 <- merge(x = tri2013, y = us.4.2013,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2013 <- fread(file = filepath.us.5.2013, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2013 <- merge(x = tri2013, y = us.5.2013,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2013[] <- lapply(tri2013[], as.factor) # converting to factor


### YEAR 2012
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012"
)

# Merging documents
filepath.us.1a.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_1a_2012.txt"
filepath.us.1b.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_1b_2012.txt"
filepath.us.2a.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_2a_2012.txt"
filepath.us.2b.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_2b_2012.txt"
filepath.us.3a.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_3a_2012.txt"
filepath.us.3b.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_3b_2012.txt"
filepath.us.3c.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_3c_2012.txt"
filepath.us.4.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_4_2012.txt"
filepath.us.5.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_5_2012.txt"
filepath.us.6.2012 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2012/US_6_2012.txt"


# facility chemical releases and other waste management
us.1a.2012 <- fread(file = filepath.us.1a.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2012
us.1b.2012 <- fread(file = filepath.us.1b.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2012 <- merge(x = us.1a.2012, y = us.1b.2012,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2012 <- fread(file = filepath.us.2a.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2012 <- merge(x = tri2012, y = us.2a.2012,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2012 <- fread(file = filepath.us.2b.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2012 <- merge(x = tri2012, y = us.2b.2012,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2012 <- fread(file = filepath.us.3a.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2012 <- merge(x = tri2012, y = us.3a.2012,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2012 <- fread(file = filepath.us.3b.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2012 <- fread(file = filepath.us.3c.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2012 <- merge(x = tri2012, y = us.3c.2012,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2012 <- fread(file = filepath.us.4.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2012 <- merge(x = tri2012, y = us.4.2012,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2012 <- fread(file = filepath.us.5.2012, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2012 <- merge(x = tri2012, y = us.5.2012,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2012[] <- lapply(tri2012[], as.factor) # converting to factor


### YEAR 2011
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011"
)

# Merging documents
filepath.us.1a.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_1a_2011.txt"
filepath.us.1b.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_1b_2011.txt"
filepath.us.2a.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_2a_2011.txt"
filepath.us.2b.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_2b_2011.txt"
filepath.us.3a.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_3a_2011.txt"
filepath.us.3b.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_3b_2011.txt"
filepath.us.3c.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_3c_2011.txt"
filepath.us.4.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_4_2011.txt"
filepath.us.5.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_5_2011.txt"
filepath.us.6.2011 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2011/US_6_2011.txt"


# facility chemical releases and other waste management
us.1a.2011 <- fread(file = filepath.us.1a.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2011
us.1b.2011 <- fread(file = filepath.us.1b.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2011 <- merge(x = us.1a.2011, y = us.1b.2011,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2011 <- fread(file = filepath.us.2a.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2011 <- merge(x = tri2011, y = us.2a.2011,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2011 <- fread(file = filepath.us.2b.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2011 <- merge(x = tri2011, y = us.2b.2011,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2011 <- fread(file = filepath.us.3a.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2011 <- merge(x = tri2011, y = us.3a.2011,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# us.3b.2011 <- fread(file = filepath.us.3b.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "")

# transfers to POTWs and merging
us.3c.2011 <- fread(file = filepath.us.3c.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X78..POTW.NAME":"X85..QUANTITY.TRANSFERRED",
           "X101..OFF.SITE.POTW.RELEASES...8.1C":"X103.OFF.SITE...POTW.RELEASES", "X110..TOTAL.TREATED"))

tri2011 <- merge(x = tri2011, y = us.3c.2011,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2011 <- fread(file = filepath.us.4.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2011 <- merge(x = tri2011, y = us.4.2011,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2011 <- fread(file = filepath.us.5.2011, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2011 <- merge(x = tri2011, y = us.5.2011,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2011[] <- lapply(tri2011[], as.factor) # converting to factor


#### YEAR 2010
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010"
)

# Merging documents
filepath.us.1a.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_1a_2010.txt"
filepath.us.1b.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_1b_2010.txt"
filepath.us.2a.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_2a_2010.txt"
filepath.us.2b.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_2b_2010.txt"
filepath.us.3a.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_3a_2010.txt"
filepath.us.3b.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_3b_2010.txt"
filepath.us.3c.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_3c_2010.txt"
filepath.us.4.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_4_2010.txt"
filepath.us.5.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_5_2010.txt"
filepath.us.6.2010 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2010/US_6_2010.txt"


# facility chemical releases and other waste management
us.1a.2010 <- fread(file = filepath.us.1a.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2010
us.1b.2010 <- fread(file = filepath.us.1b.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2010 <- merge(x = us.1a.2010, y = us.1b.2010,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2010 <- fread(file = filepath.us.2a.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2010 <- merge(x = tri2010, y = us.2a.2010,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2010 <- fread(file = filepath.us.2b.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2010 <- merge(x = tri2010, y = us.2b.2010,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2010 <- fread(file = filepath.us.3a.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2010 <- merge(x = tri2010, y = us.3a.2010,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2010 <- fread(file = filepath.us.3b.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2010 <- fread(file = filepath.us.3c.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2010 <- merge(x = tri2010, y = us.3b.2010,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2010 <- fread(file = filepath.us.4.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2010 <- merge(x = tri2010, y = us.4.2010,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2010 <- fread(file = filepath.us.5.2010, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2010 <- merge(x = tri2010, y = us.5.2010,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2010[] <- lapply(tri2010[], as.factor) # converting to factor


#### YEAR 2009
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009"
)

# Merging documents
filepath.us.1a.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_1a_2009.txt"
filepath.us.1b.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_1b_2009.txt"
filepath.us.2a.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_2a_2009.txt"
filepath.us.2b.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_2b_2009.txt"
filepath.us.3a.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_3a_2009.txt"
filepath.us.3b.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_3b_2009.txt"
filepath.us.3c.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_3c_2009.txt"
filepath.us.4.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_4_2009.txt"
filepath.us.5.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_5_2009.txt"
filepath.us.6.2009 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2009/US_6_2009.txt"


# facility chemical releases and other waste management
us.1a.2009 <- fread(file = filepath.us.1a.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2009
us.1b.2009 <- fread(file = filepath.us.1b.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2009 <- merge(x = us.1a.2009, y = us.1b.2009,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2009 <- fread(file = filepath.us.2a.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2009 <- merge(x = tri2009, y = us.2a.2009,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2009 <- fread(file = filepath.us.2b.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2009 <- merge(x = tri2009, y = us.2b.2009,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2009 <- fread(file = filepath.us.3a.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2009 <- merge(x = tri2009, y = us.3a.2009,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2009 <- fread(file = filepath.us.3b.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2009 <- fread(file = filepath.us.3c.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2009 <- merge(x = tri2009, y = us.3b.2009,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2009 <- fread(file = filepath.us.4.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2009 <- merge(x = tri2009, y = us.4.2009,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2009 <- fread(file = filepath.us.5.2009, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2009 <- merge(x = tri2009, y = us.5.2009,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2009[] <- lapply(tri2009[], as.factor) # converting to factor


#### YEAR 2008
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008"
)

# Merging documents
filepath.us.1a.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_1a_2008.txt"
filepath.us.1b.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_1b_2008.txt"
filepath.us.2a.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_2a_2008.txt"
filepath.us.2b.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_2b_2008.txt"
filepath.us.3a.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_3a_2008.txt"
filepath.us.3b.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_3b_2008.txt"
filepath.us.3c.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_3c_2008.txt"
filepath.us.4.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_4_2008.txt"
filepath.us.5.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_5_2008.txt"
filepath.us.6.2008 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2008/US_6_2008.txt"


# facility chemical releases and other waste management
us.1a.2008 <- fread(file = filepath.us.1a.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2008
us.1b.2008 <- fread(file = filepath.us.1b.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2008 <- merge(x = us.1a.2008, y = us.1b.2008,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2008 <- fread(file = filepath.us.2a.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2008 <- merge(x = tri2008, y = us.2a.2008,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2008 <- fread(file = filepath.us.2b.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2008 <- merge(x = tri2008, y = us.2b.2008,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2008 <- fread(file = filepath.us.3a.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2008 <- merge(x = tri2008, y = us.3a.2008,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2008 <- fread(file = filepath.us.3b.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2008 <- fread(file = filepath.us.3c.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2008 <- merge(x = tri2008, y = us.3b.2008,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2008 <- fread(file = filepath.us.4.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2008 <- merge(x = tri2008, y = us.4.2008,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2008 <- fread(file = filepath.us.5.2008, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2008 <- merge(x = tri2008, y = us.5.2008,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2008[] <- lapply(tri2008[], as.factor) # converting to factor


#### YEAR 2007
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007"
)

# Merging documents
filepath.us.1a.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_1a_2007.txt"
filepath.us.1b.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_1b_2007.txt"
filepath.us.2a.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_2a_2007.txt"
filepath.us.2b.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_2b_2007.txt"
filepath.us.3a.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_3a_2007.txt"
filepath.us.3b.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_3b_2007.txt"
filepath.us.3c.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_3c_2007.txt"
filepath.us.4.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_4_2007.txt"
filepath.us.5.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_5_2007.txt"
filepath.us.6.2007 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2007/US_6_2007.txt"


# facility chemical releases and other waste management
us.1a.2007 <- fread(file = filepath.us.1a.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2007
us.1b.2007 <- fread(file = filepath.us.1b.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2007 <- merge(x = us.1a.2007, y = us.1b.2007,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2007 <- fread(file = filepath.us.2a.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2007 <- merge(x = tri2007, y = us.2a.2007,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2007 <- fread(file = filepath.us.2b.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2007 <- merge(x = tri2007, y = us.2b.2007,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2007 <- fread(file = filepath.us.3a.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2007 <- merge(x = tri2007, y = us.3a.2007,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2007 <- fread(file = filepath.us.3b.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2007 <- fread(file = filepath.us.3c.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2007 <- merge(x = tri2007, y = us.3b.2007,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2007 <- fread(file = filepath.us.4.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2007 <- merge(x = tri2007, y = us.4.2007,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2007 <- fread(file = filepath.us.5.2007, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2007 <- merge(x = tri2007, y = us.5.2007,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2007[] <- lapply(tri2007[], as.factor) # converting to factor


#### YEAR 2006
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006"
)

# Merging documents
filepath.us.1a.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_1a_2006.txt"
filepath.us.1b.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_1b_2006.txt"
filepath.us.2a.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_2a_2006.txt"
filepath.us.2b.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_2b_2006.txt"
filepath.us.3a.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_3a_2006.txt"
filepath.us.3b.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_3b_2006.txt"
filepath.us.3c.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_3c_2006.txt"
filepath.us.4.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_4_2006.txt"
filepath.us.5.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_5_2006.txt"
filepath.us.6.2006 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2006/US_6_2006.txt"


# facility chemical releases and other waste management
us.1a.2006 <- fread(file = filepath.us.1a.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2006
us.1b.2006 <- fread(file = filepath.us.1b.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2006 <- merge(x = us.1a.2006, y = us.1b.2006,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2006 <- fread(file = filepath.us.2a.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2006 <- merge(x = tri2006, y = us.2a.2006,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2006 <- fread(file = filepath.us.2b.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2006 <- merge(x = tri2006, y = us.2b.2006,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2006 <- fread(file = filepath.us.3a.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2006 <- merge(x = tri2006, y = us.3a.2006,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2006 <- fread(file = filepath.us.3b.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2006 <- fread(file = filepath.us.3c.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2006 <- merge(x = tri2006, y = us.3b.2006,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2006 <- fread(file = filepath.us.4.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2006 <- merge(x = tri2006, y = us.4.2006,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2006 <- fread(file = filepath.us.5.2006, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2006 <- merge(x = tri2006, y = us.5.2006,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2006[] <- lapply(tri2006[], as.factor) # converting to factor


#### YEAR 2005
unzip(
  zipfile = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005.zip",
  exdir = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005"
)

# Merging documents
filepath.us.1a.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_1a_2005.txt"
filepath.us.1b.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_1b_2005.txt"
filepath.us.2a.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_2a_2005.txt"
filepath.us.2b.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_2b_2005.txt"
filepath.us.3a.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_3a_2005.txt"
filepath.us.3b.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_3b_2005.txt"
filepath.us.3c.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_3c_2005.txt"
filepath.us.4.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_4_2005.txt"
filepath.us.5.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_5_2005.txt"
filepath.us.6.2005 <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/US_2005/US_6_2005.txt"


# facility chemical releases and other waste management
us.1a.2005 <- fread(file = filepath.us.1a.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(
    -c("X22..MAILING.PROVINCE", contains("SIC"), contains("NPDES"),
       contains("X71"), contains("X72"), contains("X76"), contains("REVISION"),
       contains("DIOXIN"), contains("X107"), contains("X108"), contains("X110"),
       contains("X111"), contains("X112"), contains("X114"), contains("X116"),
       contains("X117"), contains("X118"), "X32..PUBLIC.CONTACT.PHONE.EXT", "X40..NAICS.ORIGIN",
       "X49..D.and.B.NR.A":"X60..RCRA.NR.J", "X119..TOTAL.DISCHARGES.TO.STREAM.A":"X169..DISCHARGES.TO.STREAM.I.....FROM.STORMWATER",
       "X172..ON.SITE.UNDERGROUND.INJ....RELEASE.POUNDS":"X177..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...RELEASE.RANGE.CODE",
       "X179..ON.SITE.UGRND.INJ.TO.CL.I.WELLS...BASIS.OF.ESTIMATE":"X181..ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...RELEASE.RANGE.CODE",
       "X183..ON.SITE.UNGRND.INJ.TO.CL.II.V.WELLS...BASIS.OF.ESTIMATE",
       "X185..ON.SITE.LANDFILLS...RELEASE.POUNDS":"X190..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...RELEASE.RANGE.CODE",
       "X192..ON.SITE.RCRA.SUBTITLE.C.LANDFILLS...BASIS.OF.ESTIMATE",
       "X193..OTHER.LANDFILLS...RELEASE.POUNDS", "X194..OTHER.LANDFILLS...RELEASE.RANGE.CODE",
       "X196..OTHER.LANDFILLS...BASIS.OF.ESTIMATE":"X198..LAND.TRTMT.APPL.FARMING...RELEASE.RANGE.CODE",
       "X200..LAND.TRTMT.APPL.FARMING...BASIS.OF.ESTIMATE":"X206..RCRA.C.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X208..RCRA.C.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X210..OTHER.SURFACE.IMPOUNDMENT...RANGE.CODE",
       "X212..OTHER.SURFACE.IMPOUNDMENT...BASIS.OF.ESTIMATE":"X214..OTHER.DISPOSAL...RANGE.CODE",
       "X216..OTHER.DISPOSAL...BASIS.OF.ESTIMATE", "X5..TITLE.OF.CERTIFYING.OFFICIAL":"X8..DATE.SIGNED",
       "X30..PUBLIC.CONTACT.NAME":"X33..PUBLIC.CONTACT.EMAIL",
       X20231015, V280
    )
  )

# chemical activities and uses. And merging with us.1a.2005
us.1b.2005 <- fread(file = filepath.us.1b.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X74..FRS.FACILITY.ID", "X76..CAS.NUMBER":"X88..REVISION.CODE.2", "X20231015"))

tri2005 <- merge(x = us.1a.2005, y = us.1b.2005,
                 by = "X75..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# source reduction activities and methods, and merging
us.2a.2005 <- fread(file = filepath.us.2a.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X64..DOCUMENT.CONTROL.NUMBER", "X95..QUANTITY.RELEASED.PRIOR.YEAR":"X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION"))

tri2005 <- merge(x = tri2005, y = us.2a.2005,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# onsite waste water treatment and merging
us.2b.2005 <- fread(file = filepath.us.2b.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X63..FRS.REGISTRY.ID", "X65..CAS.NUMBER":"X94..DIOXIN.DISTRIBUTION.17", "X20231015"))

tri2005 <- merge(x = tri2005, y = us.2b.2005,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X64..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# offsite transfers
us.3a.2005 <- fread(file = filepath.us.3a.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(-c("X1..FORM.TYPE":"X2..TRIFID", "X4..CAS.NUMBER":"X94..FRS.FACILITYY.ID", "X20231015"))

tri2005 <- merge(x = tri2005, y = us.3a.2005,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# transfers to POTWs and merging
us.3b.2005 <- fread(file = filepath.us.3b.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  # us.3c.2005 <- fread(file = filepath.us.3c.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "")
  select(c("X3..DOCUMENT.CONTROL.NUMBER", "X101..POTW.A...NAME":"X106..POTW.A...ZIP", "X94..FRS.REGISTRY.ID",
           "X95..POTW.TRANSFERS...TOTAL", "X97..POTW.RELEASES...8.1C":"X100..POTW.TRANSFERS...TREATED")) %>%
  rename(
    "X78..POTW.NAME" = "X101..POTW.A...NAME",
    "X79..POTW.ADDRESS" = "X102..POTW.A...ADDRESS",
    "X80..POTW.CITY" = "X103..POTW.A...CITY",
    "X81..POTW.STATE" = "X104..POTW.A...STATE",
    "X82..POTW.COUNTY" = "X105..POTW.A...COUNTY",
    "X83..POTW.ZIP" = "X106..POTW.A...ZIP",
    "X84..POTW.REGISTRY.ID" = "X94..FRS.REGISTRY.ID",
    "X85..QUANTITY.TRANSFERRED" = "X95..POTW.TRANSFERS...TOTAL",
    "X101..OFF.SITE.POTW.RELEASES...8.1C" = "X97..POTW.RELEASES...8.1C",
    "X102..OFF.SITE.POTW.RELEASES...8.1D" = "X98..POTW.RELEASES...8.1D",
    "X103.OFF.SITE...POTW.RELEASES" = "X90..POTW.TRANSFERS...RELEASE",
    "X110..TOTAL.TREATED" = "X100..POTW.TRANSFERS...TREATED"
  )

tri2005 <- merge(x = tri2005, y = us.3b.2005,
                 by.x = "X75..DOCUMENT.CONTROL.NUMBER", by.y = "X3..DOCUMENT.CONTROL.NUMBER",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# facility information and merging by TRIFD
us.4.2005 <- fread(file = filepath.us.4.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X4..TRIFD", "X28..SUBMITTED.PRIMARY.NAICS.CODE":"X30..SUBMITTED.INDUSTRY.NAME"))

tri2005 <- merge(x = tri2005, y = us.4.2005,
                 by.x = "X9..TRIFD", by.y = "X4..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

# additional information on disposal and other releases, source reduction  and recycling activities
us.5.2005 <- fread(file = filepath.us.5.2005, header = T, fill = T, check.names = T, sep = "\t", quote = "") %>%
  select(c("X3..TRIFD", "X84..COMMENT.TYPE":"X87..P2.CLASSIFICATION"))

tri2005 <- merge(x = tri2005, y = us.5.2005,
                 by.x = "X9..TRIFD", by.y = "X3..TRIFD",
                 all = T, no.dups = T, sort = T,
                 allow.cartesian = T)

tri2005[] <- lapply(tri2005[], as.factor) # converting to factor

tri <- bind_rows(tri2022, tri2021, tri2020, tri2019, tri2018, tri2017, tri2016, tri2015, tri2014, tri2013,
                 tri2012, tri2011, tri2010, tri2009, tri2008, tri2007, tri2006, tri2005, .id = "pid")


#=========================#
### Convert to character
#=========================#
tri[] <- lapply(tri[], as.character)
str(tri)

#====================================#
### Select the 50 US states
#====================================#
tri <- tri %>%
  filter(X14..FACILITY.STATE == "AK" |
           X14..FACILITY.STATE == "AL" |
           X14..FACILITY.STATE == "AR" |
           X14..FACILITY.STATE == "AZ" |
           X14..FACILITY.STATE == "CA" |
           X14..FACILITY.STATE == "CO" |
           X14..FACILITY.STATE == "CT" |
           X14..FACILITY.STATE == "DE" |
           X14..FACILITY.STATE == "FL" |
           X14..FACILITY.STATE == "GA" |
           X14..FACILITY.STATE == "HI" |
           X14..FACILITY.STATE == "ID" |
           X14..FACILITY.STATE == "IL" |
           X14..FACILITY.STATE == "IN" |
           X14..FACILITY.STATE == "IA" |
           X14..FACILITY.STATE == "KS" |
           X14..FACILITY.STATE == "KY" |
           X14..FACILITY.STATE == "LA" |
           X14..FACILITY.STATE == "ME" |
           X14..FACILITY.STATE == "MD" |
           X14..FACILITY.STATE == "MA" |
           X14..FACILITY.STATE == "MI" |
           X14..FACILITY.STATE == "MN" |
           X14..FACILITY.STATE == "MS" |
           X14..FACILITY.STATE == "MO" |
           X14..FACILITY.STATE == "MT" |
           X14..FACILITY.STATE == "NE" |
           X14..FACILITY.STATE == "NV" |
           X14..FACILITY.STATE == "NH" |
           X14..FACILITY.STATE == "NJ" |
           X14..FACILITY.STATE == "NM" |
           X14..FACILITY.STATE == "NY" |
           X14..FACILITY.STATE == "NC" |
           X14..FACILITY.STATE == "ND" |
           X14..FACILITY.STATE == "OH" |
           X14..FACILITY.STATE == "OK" |
           X14..FACILITY.STATE == "OR" |
           X14..FACILITY.STATE == "PA" |
           X14..FACILITY.STATE == "RI" |
           X14..FACILITY.STATE == "SC" |
           X14..FACILITY.STATE == "SD" |
           X14..FACILITY.STATE == "TN" |
           X14..FACILITY.STATE == "TX" |
           X14..FACILITY.STATE == "UT" |
           X14..FACILITY.STATE == "VT" |
           X14..FACILITY.STATE == "VA" |
           X14..FACILITY.STATE == "WA" |
           X14..FACILITY.STATE == "WV" |
           X14..FACILITY.STATE == "WI" |
           X14..FACILITY.STATE == "WY"
  )


#==============================#
### Sorting out NAICS code
#==============================#
unique(tri$X30..SUBMITTED.INDUSTRY.NAME)
sort(unique(tri$X41..PRIMARY.NAICS.CODE))

### Following 2012, 2017 and 2022 NAICS industry classifications
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^111", tri$X41..PRIMARY.NAICS.CODE)] <- "Crop Production"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^112", tri$X41..PRIMARY.NAICS.CODE)] <- "Animal Production and Aquaculture"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^113", tri$X41..PRIMARY.NAICS.CODE)] <- "Forestry and Logging"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^114", tri$X41..PRIMARY.NAICS.CODE)] <- "Fishing, Hunting and Trapping"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^115", tri$X41..PRIMARY.NAICS.CODE)] <- "Support Activities for Agriculture and Forestry"
tri$industry.category[grepl(pattern = "^11", tri$X41..PRIMARY.NAICS.CODE)] <- "Agriculture, Forestry, Fishing and Hunting"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^211", tri$X41..PRIMARY.NAICS.CODE)] <- "Oil and Gas Extraction"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^212", tri$X41..PRIMARY.NAICS.CODE)] <- "Mining (except Oil and Gas)"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^213", tri$X41..PRIMARY.NAICS.CODE)] <- "Support Activities for Mining"
tri$industry.category[grepl(pattern = "^21", tri$X41..PRIMARY.NAICS.CODE)] <- "Mining, Quarrying, and Oil and Gas Extraction"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^2211", tri$X41..PRIMARY.NAICS.CODE)] <- "Electric Utilities"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^2213", tri$X41..PRIMARY.NAICS.CODE)] <- "Water, Sewage and Other Systems"
tri$industry.category[grepl(pattern = "^22", tri$X41..PRIMARY.NAICS.CODE)] <- "Utilities"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^23", tri$X41..PRIMARY.NAICS.CODE)] <- "Construction"
tri$industry.category[grepl(pattern = "^23", tri$X41..PRIMARY.NAICS.CODE)] <- "Construction"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^311", tri$X41..PRIMARY.NAICS.CODE)] <- "Food Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^312", tri$X41..PRIMARY.NAICS.CODE)] <- "Beverage and Tobacco Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^313", tri$X41..PRIMARY.NAICS.CODE)] <- "Textile Mills Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^314", tri$X41..PRIMARY.NAICS.CODE)] <- "Textile Product Mills Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^315", tri$X41..PRIMARY.NAICS.CODE)] <- "Apparel Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^316", tri$X41..PRIMARY.NAICS.CODE)] <- "Leather and Allied Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^321", tri$X41..PRIMARY.NAICS.CODE)] <- "Wood Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^322", tri$X41..PRIMARY.NAICS.CODE)] <- "Paper Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^323", tri$X41..PRIMARY.NAICS.CODE)] <- "Printing and Related Support Activities"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^324", tri$X41..PRIMARY.NAICS.CODE)] <- "Petroleum and Coal Products Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^325", tri$X41..PRIMARY.NAICS.CODE)] <- "Chemical Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^326", tri$X41..PRIMARY.NAICS.CODE)] <- "Plastics and Rubber Products Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^327", tri$X41..PRIMARY.NAICS.CODE)] <- "Nonmetallic Mineral Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^331", tri$X41..PRIMARY.NAICS.CODE)] <- "Primary Metal Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^332", tri$X41..PRIMARY.NAICS.CODE)] <- "Forging and Stamping Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^333", tri$X41..PRIMARY.NAICS.CODE)] <- "Machinery Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^334", tri$X41..PRIMARY.NAICS.CODE)] <- "Computer and Electronic Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^335", tri$X41..PRIMARY.NAICS.CODE)] <- "Household Appliance Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^336", tri$X41..PRIMARY.NAICS.CODE)] <- "Transportation Equipment Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^337", tri$X41..PRIMARY.NAICS.CODE)] <- "Furniture and Related Product Manufacturing"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^339", tri$X41..PRIMARY.NAICS.CODE)] <- "Miscellaneous Manufacturing"
tri$industry.category[grepl(pattern = "^31", tri$X41..PRIMARY.NAICS.CODE)] <- "Manufacturing"
tri$industry.category[grepl(pattern = "^32", tri$X41..PRIMARY.NAICS.CODE)] <- "Manufacturing"
tri$industry.category[grepl(pattern = "^33", tri$X41..PRIMARY.NAICS.CODE)] <- "Manufacturing"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^42", tri$X41..PRIMARY.NAICS.CODE)] <- "Wholesale Trade"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^44", tri$X41..PRIMARY.NAICS.CODE)] <- "Retail Trade"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^45", tri$X41..PRIMARY.NAICS.CODE)] <- "Retail Trade"
tri$industry.category[grepl(pattern = "^42", tri$X41..PRIMARY.NAICS.CODE)] <- "Trade"
tri$industry.category[grepl(pattern = "^44", tri$X41..PRIMARY.NAICS.CODE)] <- "Trade"
tri$industry.category[grepl(pattern = "^45", tri$X41..PRIMARY.NAICS.CODE)] <- "Trade"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^481", tri$X41..PRIMARY.NAICS.CODE)] <- "Air Transportation"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^484", tri$X41..PRIMARY.NAICS.CODE)] <- "Truck Transportation"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^486", tri$X41..PRIMARY.NAICS.CODE)] <- "Pipeline Transportation"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^488", tri$X41..PRIMARY.NAICS.CODE)] <- "Support Activities for Transportation"
tri$industry.category[grepl(pattern = "^48", tri$X41..PRIMARY.NAICS.CODE)] <- "Transportation"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^493", tri$X41..PRIMARY.NAICS.CODE)] <- "Warehousing and Storage"
tri$industry.category[grepl(pattern = "^493", tri$X41..PRIMARY.NAICS.CODE)] <- "Postal Service"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^51", tri$X41..PRIMARY.NAICS.CODE)] <- "Information Services"
tri$industry.category[grepl(pattern = "^51", tri$X41..PRIMARY.NAICS.CODE)] <- "Information Services"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^53", tri$X41..PRIMARY.NAICS.CODE)] <- "Real Estate and Rental and Leasing Services"
tri$industry.category[grepl(pattern = "^53", tri$X41..PRIMARY.NAICS.CODE)] <- "Real Estate and Rental and Leasing Services"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^54", tri$X41..PRIMARY.NAICS.CODE)] <- "Professional, Scientific, and Technical Services"
tri$industry.category[grepl(pattern = "^54", tri$X41..PRIMARY.NAICS.CODE)] <- "Professional, Scientific, and Technical Services"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^551114", tri$X41..PRIMARY.NAICS.CODE)] <- "Corporate, Subsidiary, and Regional Managing Offices"
tri$industry.category[grepl(pattern = "^551114", tri$X41..PRIMARY.NAICS.CODE)] <- "Management of Companies and Enterprises"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^561", tri$X41..PRIMARY.NAICS.CODE)] <- "Administrative and Support Services"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^562", tri$X41..PRIMARY.NAICS.CODE)] <- "Waste Management and Remediation Services"
tri$industry.category[grepl(pattern = "^56", tri$X41..PRIMARY.NAICS.CODE)] <- "Administrative and Support and Waste Management and Remediation Services"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^611", tri$X41..PRIMARY.NAICS.CODE)] <- "Educational Services"
tri$industry.category[grepl(pattern = "^611", tri$X41..PRIMARY.NAICS.CODE)] <- "Educational Services"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^622110", tri$X41..PRIMARY.NAICS.CODE)] <- "General Medical and Surgical Hospitals"
tri$industry.category[grepl(pattern = "^622110", tri$X41..PRIMARY.NAICS.CODE)] <- "Health Care and Social Assistance"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^71", tri$X41..PRIMARY.NAICS.CODE)] <- "Arts, Entertainment, and Recreation"
tri$industry.category[grepl(pattern = "^71", tri$X41..PRIMARY.NAICS.CODE)] <- "Arts, Entertainment, and Recreation"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^81", tri$X41..PRIMARY.NAICS.CODE)] <- "Other Services (except Public Administration)"
tri$industry.category[grepl(pattern = "^81", tri$X41..PRIMARY.NAICS.CODE)] <- "Other Services (except Public Administration)"

tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^921", tri$X41..PRIMARY.NAICS.CODE)] <- "Executive, Legislative, and Other General Government Support"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^922", tri$X41..PRIMARY.NAICS.CODE)] <- "Justice, Public Order, and Safety Activities"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^923", tri$X41..PRIMARY.NAICS.CODE)] <- "Administration of Human Resource Programs"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^924", tri$X41..PRIMARY.NAICS.CODE)] <- "Administration of Environmental Quality Programs"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^926", tri$X41..PRIMARY.NAICS.CODE)] <- "Administration of Economic Programs"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^927", tri$X41..PRIMARY.NAICS.CODE)] <- "Space Research and Technology"
tri$X30..SUBMITTED.INDUSTRY.NAME[grepl(pattern = "^928", tri$X41..PRIMARY.NAICS.CODE)] <- "National Security and International Affairs"
tri$industry.category[grepl(pattern = "^92", tri$X41..PRIMARY.NAICS.CODE)] <- "Public Administration"
unique(tri$industry.category)

## Sorting out the NAs in the NAICs names
unique(tri$X30..SUBMITTED.INDUSTRY.NAME)

tri.naics.na <- tri[is.na(tri$X30..SUBMITTED.INDUSTRY.NAME),]
tri.naics.na <- tri.naics.na %>%
  select(c(X41..PRIMARY.NAICS.CODE, X28..SUBMITTED.PRIMARY.NAICS.CODE,
           X30..SUBMITTED.INDUSTRY.NAME, X29..SUBMITTED.INDUSTRY.CODE))
View(tri.naics.na)

tri <- tri[!is.na(tri$X30..SUBMITTED.INDUSTRY.NAME)] # remove NAs in the column Naics NAME. They are only 9 rows
tri <- tri[tri$X30..SUBMITTED.INDUSTRY.NAME != "Other",] # remove unclassifiable industries. They are only 3 rows
unique(tri$X30..SUBMITTED.INDUSTRY.NAME)
sort(unique(tri$X41..PRIMARY.NAICS.CODE))
unique(tri[tri$X41..PRIMARY.NAICS.CODE == "212321",]$X30..SUBMITTED.INDUSTRY.NAME)

write_rds(x = tri, file = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/tri.rds", compress = "xz")


#=========================#
### Loading dataset
#=========================#
filepath <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/tri.rds"
tri <- readRDS(file = filepath)

#==================================================================================================================#
### Sorting the columns
# List of removed columns: X75..DOCUMENT.CONTROL.NUMBER, X16..BIA.CODE:X23..MAILING.ZIP.CODE,
# X42..NAICS.CODE.2:X46..NAICS.CODE.6, X220..WASTE.ROCK.QUANTITY, X229..OFF.SITE...UNDERGROUND.INJECTION,
# X233..OFF.SITE...SURFACE.IMPOUNDMENT, X95..QUANTITY.RELEASED.PRIOR.YEAR:X138..QUANTITY.TREATED.OFF.SITE.SECOND.FOLLOWING.YEAR
# List of removed rows: Form A
#==================================================================================================================#
tri.A <- tri %>% filter(X1..FORM.TYPE == "A")
tri.R <- tri %>% filter(X1..FORM.TYPE == "R")
tri <- tri.R

tri <- tri %>%
  select(
    c(
      pid, X9..TRIFD, X2..REPORTING.YEAR:X15..FACILITY.ZIP.CODE, X47..LATITUDE, X48..LONGITUDE, X48..LONGITUDE,
      X24..ENTIRE.FACILITY.IND:X41..PRIMARY.NAICS.CODE, X30..SUBMITTED.INDUSTRY.NAME:industry.category,

      # onsite releases and treatments and methods of treatment - 1a
      X73..STANDARDIZED.PARENT.COMPANY.NAME:X219..WASTE.ROCK.PILE.MANAGED.IND,
      X264..ENERGY.RECOVERY.ON.SITE.CURRENT.YEAR:X278..ON.SITE.RECYCLING.PROCESSES.METHOD.7,

      # offsite releases and treatment and methods of treatment - 1a
      X221..OFF.SITE...POTW.RELEASES.81C:X228..OFF.SITE...WASTEWATER.TREATMENT..EXCLUDING.POTWS..METALS.AND.METAL.COMPOUNDS.ONLY,
      X230..OFF.SITE...UNDERGROUND.INJECTION...CLASS.1.WELLS:X232..OFF.SITE...LANDFILLS.DISPOSAL.SURFACE.IMPOUNDMENTS,
      X234..OFF.SITE...RCRA.SUBTITLE.C.SURFACE.IMPOUNDMENTS:X263..TOTAL.POTW.TRANSFER,

      # chemical activities and uses - 1b
      X89..PRODUCE.THE.CHEMICAL:X142..Z399..OTHER,

      # source reduction activities
      X139..CATASTROPHIC.RELEASES.OR.OTHER.ONE.TIME.EVENTS:X151..EST.ANNUAL.REDUCTION...FIRST.SOURCE.REDUCTION.ACTIVITY...CODE.DESCRIPTION,

      # onsite waste treatment - 2b
      X95..STREAM.1...WASTE.STREAM.CODE:X159..STREAM.5...EFFICIENCY.RANGE.CODE,

      # offsite waste treatment - 3a
      X95..OFFSITE.RCRA.ID.NR:X106..FRS.ID...TRANSFER.LOCATION,

      #POTW transfers - 3b
      X78..POTW.NAME:X84..POTW.REGISTRY.ID
    )
  )


#==================================================================================================================#
### Converting to columns numeric, definining variables
#==================================================================================================================#
tri$X106..MAXIMUM.AMOUNT.ON.SITE[tri$X106..MAXIMUM.AMOUNT.ON.SITE == "NV"] <- 0
tri <- tri %>%
  mutate(
    # onsite geolocation
    pid = pid,
    year = as.numeric(as.character(X2..REPORTING.YEAR)),
    triid = X9..TRIFD,
    facility.id = X74..FRS.FACILITY.ID,
    facility.city = X12..FACILITY.CITY,
    facility.county = X13..FACILITY.COUNTY,
    facility.state = X14..FACILITY.STATE,
    facility.zipcode = X15..FACILITY.ZIP.CODE,
    facility.latitude = X47..LATITUDE,
    facility.longitude = X48..LONGITUDE,

    # offsite geolocations
    offsite.id = X95..OFFSITE.RCRA.ID.NR,
    offsite.facility.id = X106..FRS.ID...TRANSFER.LOCATION,
    offsite.sequence.number = X96..OFFSITE.TRANSFER.SEQUENCE.NUMBER,
    offsite.city = X99..OFFSITE.CITY,
    offsite.county = X100..OFFSITE.COUNTY,
    offsite.state = X101..OFFSITE.STATE,
    offsite.province = X102..OFFSITE.PROVINCE,
    offsite.zipcode = X103..OFFSITE.ZIPCODE,
    offsite.countryid = X104..OFFSITE.COUNTRY.ID,

    # potw geolocations
    potw.id = X84..POTW.REGISTRY.ID,
    potw.city = X80..POTW.CITY,
    potw.state = X81..POTW.STATE,
    potw.county = X82..POTW.COUNTY,
    potw.zipcode = X83..POTW.ZIP,

    # chemicals
    naics.code = X41..PRIMARY.NAICS.CODE,
    industry.name = X30..SUBMITTED.INDUSTRY.NAME,
    industry.category = industry.category,
    chemical.id = X77..TRI.CHEMICAL.ID,
    chemical.name = X78..CHEMICAL.NAME,
    mixture = X79..MIXTURE.NAME,
    chemical.classification = X81..CLASSIFICATION,
    unit.of.measure = X82..UNIT.OF.MEASURE,

    # Outcome Variables
    ## onsite releases and treatments
    # air emissions
    total.fug.air.emissions.onsite = as.numeric(as.character(X109..TOTAL.FUGITIVE.AIR.EMISSIONS)),
    total.point.air.emissions.onsite = as.numeric(as.character(X113..TOTAL.STACK.AIR.EMISSIONS)),
    total.air.emissions.onsite = as.numeric(as.character(X115..TOTAL.AIR.EMISSIONS)),
    # water pollution/releases
    total.num.receiving.streams.onsite = as.numeric(as.character(X170..TOTAL.NUMBER.OF.RECEIVING.STREAMS)),
    total.surface.water.discharge.onsite = as.numeric(as.character(X171..TOTAL.SURFACE.WATER.DISCHARGE)),
    # land pollution/releases
    total.underground.injection.I.wells.onsite = as.numeric(as.character(X178..TOTAL.ON.SITE.UGRND.INJ.TO.CL.I.WELLS...POUNDS)),
    total.underground.injection.I.IV.wells.onsite = as.numeric(as.character(X182..TOTAL.ON.SITE.UGRND.INJ.TO.CL.II.V.WELLS...POUNDS)),
    total.underground.injection.onsite = as.numeric(as.character(X184..TOTAL.ON.SITE.UNDERGROUND.INJECTION)),
    total.landfills1.onsite = as.numeric(as.character(X191..TOTAL.ON.SITE.RCRA.SUBTITLE.C.LANDFILLS)),
    total.landfills2.onsite = as.numeric(as.character(X195..TOTAL.OTHER.ON.SITE.LANDFILLS)),
    total.landfills.onsite = total.landfills1.onsite + total.landfills2.onsite,
    total.releases.toland.treatment.onsite = as.numeric(as.character(X199..TOTAL.ON.SITE.LAND.TREATMENT)),
    total.surface.impoundment1.onsite = as.numeric(as.character(X207..TOTAL.RCRA.C.SURFACE.IMPOUNDMENTS)),
    total.surface.impoundment2.onsite = as.numeric(as.character(X211..TOTAL.OTHER.SURFACE.IMPOUNDMENTS)),
    total.surface.impoundment.onsite = total.surface.impoundment1.onsite + total.surface.impoundment2.onsite,
    total.land.releases.other.onsite = as.numeric(as.character(X215..TOTAL.OTHER.DISPOSAL)),
    total.land.releases.onsite = as.numeric(as.character(X217..TOTAL.ON.SITE.LAND.RELEASES)),
    # all onsite releases
    total.releases.onsite = as.numeric(as.character(X218..TOTAL.ON.SITE.RELEASES)),

    ## onsite waste management
    # energy recovery
    energy.recovery.onsite = as.numeric(as.character(X264..ENERGY.RECOVERY.ON.SITE.CURRENT.YEAR)),
    industrial.kiln.onsite = case_when(X268..ON.SITE.ENERGY.RECOVERY.METHOD.1 == "U01" |
                                         X269..ON.SITE.ENERGY.RECOVERY.METHOD.2 == "U01" |
                                         X270..ON.SITE.ENERGY.RECOVERY.METHOD.3 == "U01" ~ 1, T ~ 0),
    industrial.furnace.onsite = case_when(X268..ON.SITE.ENERGY.RECOVERY.METHOD.1 == "U02" |
                                            X269..ON.SITE.ENERGY.RECOVERY.METHOD.2 == "U02" |
                                            X270..ON.SITE.ENERGY.RECOVERY.METHOD.3 == "U02" ~ 1, T ~ 0),
    industrial.boiler.onsite = case_when(X268..ON.SITE.ENERGY.RECOVERY.METHOD.1 == "U03" |
                                           X269..ON.SITE.ENERGY.RECOVERY.METHOD.2 == "U03" |
                                           X270..ON.SITE.ENERGY.RECOVERY.METHOD.3 == "U03" ~ 1, T ~ 0),

    # recycling
    recycling.onsite = as.numeric(as.character(X265..RECYCLED.ON.SITE.CURRENT.YEAR)),
    metal.recovery.onsite = case_when(
      # (by retorting, smelting, or chemical or physical extraction)
      X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "H10" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R27" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R29" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R30" |
        X273..ON.SITE.RECYCLING.PROCESSES.METHOD.2 == "R29" ~ 1, T ~ 0),
    solvent.recovery.onsite = case_when(
      # (including distillation, evaporation, fractionation, or extraction)
      X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "H20" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R11" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R12" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R14" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R19" |
        X273..ON.SITE.RECYCLING.PROCESSES.METHOD.2 == "H20" ~ 1, T ~ 0),
    reuse.onsite = case_when(
      # (including acid regeneration or other chemical reaction process)
      X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "H39" |
        X272..ON.SITE.RECYCLING.PROCESSES.METHOD.1 == "R99" |
        X273..ON.SITE.RECYCLING.PROCESSES.METHOD.2 == "H39" |
        X274..ON.SITE.RECYCLING.PROCESSES.METHOD.3 == "H39" ~ 1, T ~ 0),

    # treatment
    treatment.onsite = as.numeric(as.character(X266..TREATED.ON.SITE.CURRENT.YEAR)),
    air.emissions.treatment.onsite = case_when(
      # air emissions treatment include A01 - Flare, A02 - Condenser, A03 - Scrubber, A04 - Absorber,
      # A05 Electrostatic Precipitator, A06 - Mechanical Separation, A07 - Other Air Emission Treatment
      X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A01" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A01" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A01" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A01" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A01" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A01" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A01" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A01" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A02" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A02" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A02" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A02" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A02" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A02" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A02" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A02" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A03" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A03" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A03" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A03" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A03" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A03" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A03" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A03" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A04" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A04" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A04" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A04" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A04" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A04" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A04" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A04" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A05" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A05" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A05" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A05" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A05" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A05" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A05" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A05" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A06" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A06" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A06" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A06" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A06" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A06" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A06" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A06" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "A07" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "A07" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "A07" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "A07" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "A07" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "A07" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "A07" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "A07" ~ 1, T ~ 0),

    biological.treatment.onsite = case_when(
      # H081 - Biological treatment with or without precipitation including aerobic, anaerobic, facultative and
      # other biological treatments
      X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H081" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H081" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H081" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H081" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H081" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H081" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H081" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H081" |

        # aerobic
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "B11" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "B11" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "B11" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "B11" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "B11" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "B11" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "B11" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "B11" |

        #anaerobic
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "B21" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "B21" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "B21" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "B21" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "B21" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "B21" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "B21" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "B21" |

        # facultative
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "B31" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "B31" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "B31" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "B31" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "B31" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "B31" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "B31" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "B31" |

        # other biological treatment
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "B99" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "B99" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "B99" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "B99" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "B99" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "B99" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "B99" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "B99" ~ 1, T ~ 0),

    chemical.treatment.onsite = case_when(
      # includes C01, C02, C09, C11, C21, C31, C41:C46, C99. The new codes are H071, H073, H075, H077, H121, and H129.
      # chemical reduction with/without precipitation - H071
      X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H071" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H071" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H071" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H071" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H071" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H071" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H071" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H071" |

        # Cyanide destruction with/without precipitation - H073
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H073" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H073" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H073" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H073" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H073" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H073" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H073" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H073" |

        # chemical oxidation - H075
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H075" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H075" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H075" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H075" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H075" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H075" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H075" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H075" |

        # Other chemical precipitation with or without pre- treatment - H077
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H077" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H077" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H077" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H077" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H077" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H077" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H077" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H077" |

        # neutralisation - H121
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H121" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H121" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H121" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H121" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H121" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H121" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H121" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H121" |

        # Other treatment - H129
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H129" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H129" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H129" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H129" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H129" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H129" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H129" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H129" |

        #old codes
        # chemical reduction with/without precipitation - H071 = C01 and C02
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C01" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C01" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C01" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C01" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C01" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C01" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C01" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C01" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C02" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C02" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C02" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C02" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C02" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C02" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C02" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C02" |

        # Cyanide destruction with/without precipitation - H073 = C41:C43
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C41" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C41" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C41" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C41" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C41" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C41" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C41" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C41" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C42" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C42" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C42" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C42" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C42" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C42" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C42" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C42" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C43" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C43" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C43" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C43" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C43" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C43" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C43" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C43" |

        # chemical oxidation - H075 = C44:C46
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C44" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C44" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C44" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C44" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C44" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C44" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C44" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C44" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C45" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C45" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C45" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C45" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C45" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C45" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C45" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C45" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C46" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C46" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C46" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C46" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C46" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C46" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C46" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C46" |

        # Other chemical precipitation with or without pre-treatment - H077 = C09
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C09" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C09" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C09" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C09" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C09" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C09" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C09" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C09" |

        # neutralisation - H121 = C11
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C11" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C11" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C11" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C11" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C11" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C11" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C11" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C11" |

        # Other treatment - H129 = C99
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "C99" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "C99" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "C99" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "C99" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "C99" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "C99" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "C99" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "C99" ~ 1, T ~ 0),

    incineration.thermal.treatment.onsite = case_when(
      # includes F01, F11, F19, F31, F41, F42, F51, F61, F71, F81:F83 and F99. The new codes are H040, H076 and H122.
      # Incineration B thermal destruction other than use as a fuel - H040
      X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H040" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H040" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H040" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H040" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H040" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H040" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H040" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H040" |

        # Wet air oxidation - H076
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H076" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H076" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H076" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H076" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H076" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H076" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H076" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H076" |

        # Evaporation - H122
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H122" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H122" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H122" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H122" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H122" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H122" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H122" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H122" |

        # The old codes
        # Incineration B thermal destruction other than use as a fuel -
        # H040 = F01, F11, F19, F31, F41, F42, F51, F61, F71, F81 and F99
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F01" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F01" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F01" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F01" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F01" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F01" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F01" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F01" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F11" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F11" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F11" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F11" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F11" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F11" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F11" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F11" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F19" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F19" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F19" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F19" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F19" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F19" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F19" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F19" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F31" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F31" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F31" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F31" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F31" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F31" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F31" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F31" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F41" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F41" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F41" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F41" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F41" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F41" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F41" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F41" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F42" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F42" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F42" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F42" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F42" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F42" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F42" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F42" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F51" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F51" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F51" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F51" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F51" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F51" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F51" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F51" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F61" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F61" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F61" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F61" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F61" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F61" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F61" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F61" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F71" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F71" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F71" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F71" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F71" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F71" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F71" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F71" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F81" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F81" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F81" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F81" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F81" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F81" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F81" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F81" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F99" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F99" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F99" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F99" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F99" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F99" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F99" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F99" |

        # wet air oxidation - H076 = F82
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F82" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F82" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F82" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F82" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F82" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F82" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F82" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F82" |

        # wet air oxidation - H122 = F83
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "F83" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "F83" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "F83" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "F83" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "F83" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "F83" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "F83" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "F83" ~ 1, T ~ 0),

    physical.treatment.onsite = case_when(
      # includes P01, P09, F19, P11:P19, P21:P23, P29, P31, P41, P42, P49, P51, P61, and P99.
      # The new codes are H082, H083, H101, H123, H124 and H129
      # Adsorption - H082
      X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H082" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H082" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H082" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H082" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H082" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H082" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H082" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H082" |

        # Air or steam stripping - H083
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H083" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H083" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H083" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H083" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H083" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H083" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H083" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H083" |

        # Sludge treatment and/or dewatering - H101
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H101" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H101" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H101" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H101" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H101" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H101" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H101" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H101" |

        # Settling or clarification - H123
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H123" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H123" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H123" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H123" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H123" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H123" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H123" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H123" |

        # Phase separation - H124
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H124" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H124" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H124" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H124" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H124" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H124" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H124" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H124" |

        # Other treatment - H129
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "H129" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "H129" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "H129" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "H129" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "H129" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "H129" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "H129" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "H129" |

        # The old codes are:
        # P01, P09, F19, P11:P19, P21:P23, P29, P31, P41, P42, P49, P51, P61, and P99.
        # Other treatment - H129 = P01, P09, P31, P51, P61 and P99.
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P01" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P01" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P01" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P01" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P01" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P01" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P01" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P01" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P09" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P09" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P09" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P09" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P09" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P09" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P09" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P09" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P31" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P31" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P31" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P31" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P31" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P31" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P31" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P31" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P51" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P51" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P51" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P51" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P51" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P51" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P51" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P51" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P61" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P61" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P61" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P61" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P61" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P61" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P61" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P61" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P99" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P99" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P99" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P99" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P99" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P99" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P99" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P99" |

        # Adsorption - H082 = P21:P23, P29
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P21" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P21" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P21" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P21" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P21" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P21" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P21" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P21" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P22" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P22" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P22" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P22" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P22" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P22" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P22" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P22" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P23" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P23" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P23" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P23" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P23" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P23" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P23" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P23" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P29" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P29" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P29" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P29" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P29" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P29" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P29" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P29" |

        # Air or steam stripping - H083 = P41, P42, P49
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P41" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P41" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P41" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P41" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P41" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P41" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P41" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P41" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P42" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P42" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P42" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P42" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P42" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P42" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P42" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P42" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P49" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P49" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P49" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P49" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P49" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P49" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P49" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P49" |

        # Sludge treatment and/or dewatering - H101 = P13
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P13" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P13" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P13" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P13" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P13" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P13" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P13" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P13" |

        # Settling or clarification - H123 = P11 and P12
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P11" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P11" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P11" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P11" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P11" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P11" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P11" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P11" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P12" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P12" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P12" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P12" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P12" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P12" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P12" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P12" |

        # Phase separation - H124 = P14:P19
        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P14" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P14" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P14" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P14" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P14" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P14" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P14" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P14" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P15" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P15" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P15" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P15" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P15" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P15" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P15" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P15" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P16" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P16" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P16" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P16" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P16" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P16" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P16" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P16" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P17" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P17" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P17" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P17" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P17" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P17" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P17" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P17" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P18" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P18" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P18" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P18" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P18" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P18" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P18" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P18" |

        X96..STREAM.1...TRTMT.METHOD...SEQUENCE.1 == "P19" |
        X97..STREAM.1...TRTMT.METHOD...SEQUENCE.2 == "P19" |
        X98..STREAM.1...TRTMT.METHOD...SEQUENCE.3 == "P19" |
        X99..STREAM.1...TRTMT.METHOD...SEQUENCE.4 == "P19" |
        X100..STREAM.1...TRTMT.METHOD...SEQUENCE.5 == "P19" |
        X101..STREAM.1...TRTMT.METHOD...SEQUENCE.6 == "P19" |
        X102..STREAM.1...TRTMT.METHOD...SEQUENCE.7 == "P19" |
        X103..STREAM.1...TRTMT.METHOD...SEQUENCE.8 == "P19" ~ 1, T ~ 0),

    ## overall onsite
    total.waste.management.onsite = as.numeric(as.character(X267..TOTAL.ON.SITE.WASTE.MANAGEMENT)), #sum X264 through X266

    ## Other onsite mechanisms - onsite Source or Pollution Reduction Activities
    material.subandmod = case_when(
      # Material Substitutions and Modifications refer to changing input purity or dimensions, or replacing a raw material,
      # feedstock, reagent, or other substance with environmentally preferable alternatives.
      X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S01" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S02" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S03" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S04" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S05" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S06" ~ 1, T ~ 0),
    sub.fuel.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S01" ~ 1, T ~ 0),
    sub.organic.solvent.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S02" ~ 1, T ~ 0),
    sub.rawm.feedstock.reactchem.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S03" ~ 1, T ~ 0),
    sub.manu.proccess.ancilliary.chems.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S04" ~ 1, T ~ 0),
    mod.content.grade.purity.chems.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S05" ~ 1, T ~ 0),
    other.matmods.matsubmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S06" ~ 1, T ~ 0),

    product.modification = case_when(
      # Product Modifications refer to changing the end product through design, composition, formulation, or packaging
      # changes, as well as full final product replacements that reduce the generation of waste.
      X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S11" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S12" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S13" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S14" ~ 1, T ~ 0),
    devd.newproductline.pmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S11" ~ 1, T ~ 0),
    alt.dim.comp.design.pmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S12" ~ 1, T ~ 0),
    mod.packaging.pmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S13" ~ 1, T ~ 0),
    other.pmods.pmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S14" ~ 1, T ~ 0),

    process.equip.modification = case_when(
      # Process and Equipment Modifications refer to improvements to industrial processes and/or associated equipment
      # including implementation of new processes that produce less waste, direct reuse of chemicals, or technological changes
      # impacting synthesis, formulation, fabrication, and assembly, and surface treatment such as cleaning, degreasing, surface
      # preparation, and finishing.
      X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S21" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S22" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S23" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S24" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S25" ~ 1, T ~ 0),
    optimised.process.efficiency.pequipmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S21" ~ 1, T ~ 0),
    recirculationinprocess.pequipmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S22" ~ 1, T ~ 0),
    newtech.technique.process.pequipmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S23" ~ 1, T ~ 0),
    equipment.upgrade.update.pequipmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S24" ~ 1, T ~ 0),
    other.pequipmods.pequipmod = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S25" ~ 1, T ~ 0),

    inventory.material.mgt = case_when(
      # Inventory and Material Management refers to improvements in procurement, inventory tracking, preventative
      # monitoring, and storage and handling of chemicals and materials as they move through a facility to optimize their use
      # and prevent spills and leaks during operation.
      X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S31" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S32" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S33" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S34" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S35" ~ 1, T ~ 0),
    better.labelling.testing.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S31" ~ 1, T ~ 0),
    containers.sizechange.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S32" ~ 1, T ~ 0),
    improved.materialhandling.operations.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S33" ~ 1, T ~ 0),
    improved.monitoring.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S34" ~ 1, T ~ 0),
    other.immgts.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S35" ~ 1, T ~ 0),

    operating.practices.training = case_when(
      # Operating Practices and Training refers to improvements in maintenance, production scheduling, process monitoring,
      # and other practices that enhance operator expertise and housekeeping measures that eliminate or minimize waste
      X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S41" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S42" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S43" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S44" |
        X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S45" ~ 1, T ~ 0),
    improved.schdule.operation.procedures.opt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S41" ~ 1, T ~ 0),
    changed.production.schedule.opt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S42" ~ 1, T ~ 0),
    intro.inline.productquality.process.analysis.opt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S43" ~ 1, T ~ 0),
    improved.monitoring.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S44" ~ 1, T ~ 0),
    other.immgts.immgt = case_when(X142..FIRST..SOURCE.REDUCTION.ACTIVITY.CODE == "S45" ~ 1, T ~ 0),


    ### offsite releases and treatments
    # land pollution/releases
    potw.releases.underground.Iwells.offsite = as.numeric(as.character(X221..OFF.SITE...POTW.RELEASES.81C)),
    potw.releases.underground.other.offsite = as.numeric(as.character(X222..OFF.SITE...POTW.RELEASES.81D)),
    total.potw.releases.offsite = as.numeric(as.character(X223..OFF.SITE...POTW.RELEASES)),
    total.underground.injection.I.wells.offsite = as.numeric(as.character(X230..OFF.SITE...UNDERGROUND.INJECTION...CLASS.1.WELLS)),
    total.underground.injection.I.IV.wells.offsite = as.numeric(as.character(X231..OFF.SITE...UNDERGROUND.INJECTION...CLASS.II.V.WELLS)),
    total.underground.injection.offsite = total.underground.injection.I.wells.offsite + total.underground.injection.I.IV.wells.offsite,
    total.landfills1.offsite = as.numeric(as.character(X232..OFF.SITE...LANDFILLS.DISPOSAL.SURFACE.IMPOUNDMENTS)),
    total.landfills2.offsite = as.numeric(as.character(X236..OFF.SITE...OTHER.LANDFILLS)),
    total.landfills3.offsite = as.numeric(as.character(X237..OFF.SITE...RCRA.SUBTITLE.C.LANDFILLS)),
    total.landfills.offsite = total.landfills1.offsite +
      total.landfills2.offsite +
      total.landfills3.offsite,
    total.releases.toland.treatment.offsite = as.numeric(as.character(X238..OFF.SITE...DISPOSAL...LAND.TREATMENT)),
    total.surface.impoundment1.offsite = as.numeric(as.character(X234..OFF.SITE...RCRA.SUBTITLE.C.SURFACE.IMPOUNDMENTS)),
    total.surface.impoundment2.offsite = as.numeric(as.character(X235..OFF.SITE...OTHER.SURFACE.IMPOUNDMENTS)),
    total.surface.impoundment.offsite = total.surface.impoundment2.offsite + total.surface.impoundment2.offsite,
    total.land.releases.other.offsite = as.numeric(as.character(X239..OFF.SITE...DISPOSAL...OTHER.LAND.DISPOSAL)),
    total.land.releases.offsite = total.underground.injection.offsite +
      total.landfills.offsite +
      total.releases.toland.treatment.offsite +
      total.surface.impoundment.offsite +
      total.land.releases.other.offsite,
    # storage only
    total.releases.storage.offsite = as.numeric(as.character(X224..OFF.SITE...STORAGE.ONLY)),
    total.releases.metalsolidify1.offsite = as.numeric(as.character(X225..OFF.SITE...SOLIDIFICATION.STABILIZATION....METALS.AND.METAL.COMPOUNDS.ONLY)),
    total.releases.metalsolidify2.offsite = as.numeric(as.character(X227..OFF.SITE...SOLIDIFICATION.STABLIZATION...RELEASE...METALS.AND.METAL.COMPOUNDS.ONLY)),
    total.releases.metalsolidify.offsite = total.releases.metalsolidify1.offsite + total.releases.metalsolidify1.offsite,
    # water pollution/releases
    total.wastewater.releases1.npotw.offsite = as.numeric(as.character(X226..OFF.SITE...WASTEWATER.TREATMENT.RELEASE..EXCLUDING.POTWs....METALS.AND.METAL.COMPOUNDS.ONLY)),
    total.wastewater.releases2.npotw.offsite = as.numeric(as.character(X228..OFF.SITE...WASTEWATER.TREATMENT..EXCLUDING.POTWS..METALS.AND.METAL.COMPOUNDS.ONLY)),
    total.wastewater.releases.npotw.offsite = total.wastewater.releases1.npotw.offsite + total.wastewater.releases2.npotw.offsite,
    # all offsite releases
    total.releases.other.mgt.offsite = as.numeric(as.character(X240..OFF.SITE...DISPOSAL...OTHER.OFF.SITE.MANAGEMENT)),
    total.releases.wastebroker1.offsite = as.numeric(as.character(X241..OFF.SITE...DISPOSAL...TRANSFER.TO.WASTE.BROKER)),
    total.releases.wastebroker2.offsite = as.numeric(as.character(X261..OFF.SITE...TRANSFER.TO.WASTE.BROKER)),
    total.releases.wastebroker.offsite = total.releases.wastebroker1.offsite + total.releases.wastebroker2.offsite,
    total.releases.unknown.offsite = as.numeric(as.character(X242..OFF.SITE...DISPOSAL...UNKNOWN)),
    total.releases.offsite = as.numeric(as.character(X243..TOTAL.TRANSFERRED.OFF.SITE.FOR.DISPOSAL)),

    ## offsite waste management
    # energy recovery
    energy.recovery.offsite = as.numeric(as.character(X250..OFF.SITE...ENERGY.RECOVERY)),
    energy.recovery.wastebroker.offsite = as.numeric(as.character(X251..OFF.SITE...TRANSFER.TO.WASTE.BROKER.FOR.ENERGY.RECOVERY)),
    total.energy.recovery.offsite = as.numeric(as.character(X252..TOTAL.TRANSFERRED.OFF.SITE.FOR.ENERGY.RECOVERY)),

    # recycling
    recycling.solventsorganic.offsite = as.numeric(as.character(X244..OFF.SITE...RECYCLING...SOLVENTS.ORGANICS.RECOVERY)),
    recycling.metals.offsite = as.numeric(as.character(X245..OFF.SITE...RECYCLING..METALS.RECOVERY)),
    recycling.reuse.offsite = as.numeric(as.character(X246..OFF.SITE...RECYCLING...OTHER.REUSE.OR.RECOVERY)),
    recycling.acidgen.offsite = as.numeric(as.character(X247..OFF.SITE...RECYCLING...ACID.REGENERATION)),
    recycling.wastebroker.offsite = as.numeric(as.character(X248..OFF.SITE...RECYCLING...TRANSFER.TO.WASTE.BROKER)),
    total.recycling.offsite = as.numeric(as.character(X249..TOTAL.TRANSFERRED.OFF.SITE.FOR.RECYCLING)),

    # treatment
    potw.treatment.offsite = as.numeric(as.character(X253..OFF.SITE...POTW.TREATMENT)),
    treatment.nonmetalsolidify.offsite = as.numeric(as.character(X254..OFF.SITE...SOLIDIFICATION.STABILIZATION.TREATMENT..NON.METALS)),
    incineration.thermal.treatment.offsite = as.numeric(as.character(X255..OFF.SITE..INCINERATION.THERMAL.TREATMENT)),
    incineration.thermal.treatment.heatvalue.offsite = as.numeric(as.character(X256..OFF.SITE...INCINERATION.INSIGNIFICANT.HEAT.VALUE)),
    wastewater.treatment.nonmetals.offsite = as.numeric(as.character(X257..OFF.SITE...WASTEWATER.TREATMENT..EXCLUDING.POTWs....NON.METALS.ONLY)),
    waste.treatment.other.offsite = as.numeric(as.character(X258..OFF.SITE...OTHER.WASTE.TREATMENT)),
    waste.treatment.wastebroker.offsite = as.numeric(as.character(X259..OFF.SITE....TRANSFER.TO.WASTE.BROKER...WASTE.TREATMENT)),
    total.waste.treatment.offsite = as.numeric(as.character(X260..TOTAL.TRANSFERRED.OFF.SITE.FOR.TREATMENT)),

    ## overall offsite
    total.potw.management.offsite = as.numeric(as.character(X263..TOTAL.POTW.TRANSFER)), #sum X223 and X253
    total.waste.management.offsite = as.numeric(as.character(X262..TOTAL.TRANSFERRED.OFF.SITE.FOR.FURTHER.WASTE.MANAGEMENT)), #sums X243, X249, X252 and X260


    ### placebo
    total.release.onsite.catastrophicevents = as.numeric(as.character(X139..CATASTROPHIC.RELEASES.OR.OTHER.ONE.TIME.EVENTS)),

    ### Numeric regressors and others
    ## onsite regressors
    maxnum.chem.onsite = as.numeric(as.character(X106..MAXIMUM.AMOUNT.ON.SITE)),
    trade.secret = case_when(X3..TRADE.SECRET.IND == "YES" ~ 1, T ~ 0),
    sanitised = case_when(X4..SANITIZED.IND == "YES" ~ 1, T ~ 0),
    entire.facility = case_when(X24..ENTIRE.FACILITY.IND == "YES" ~ 1, T ~ 0),
    federal.facility = case_when(X26..FEDERAL.FACILITY.IND == "YES" ~ 1, T ~ 0),
    govt.owned.facility = case_when(X27..GOCO.FACILITY.IND == "YES" ~ 1, T ~ 0),
    entire.facility = case_when(X24..ENTIRE.FACILITY.IND == "YES" ~ 1, T ~ 0),
    comment.type = X84..COMMENT.TYPE,
    comment.type.description = X85..COMMENT.TYPE.DESCRIPTION,
    comment.text = X86..COMMENT.TEXT,
    classification = X87..P2.CLASSIFICATION,
    elemental.metal.included = case_when(X80..ELEMENTAL.METAL.INCLUDED == "YES" ~ 1, T ~ 0),
    clean.air.act.chems = case_when(X83..CLEAN.AIR.ACT.IND == "YES" ~ 1, T ~ 0),
    carcinogenic.chems = case_when(X84..CARCINOGEN.IND == "YES" ~ 1, T ~ 0),
    pfas.chems = case_when(X85..PFAS.IND == "YES" ~ 1, T ~ 0),
    metal.restrict.tri = case_when(X86..METAL.IND == "YES" ~ 1, T ~ 0),
    production.ratio.activity.index = as.numeric(as.character(X140..PROD.RATIO.ACTIVITY.INDEX)),
    production.or.activity = X141..PROD.RATIO.OR.ACTIVITY,
    # chemical uses
    produced.chem.facility = case_when(X89..PRODUCE.THE.CHEMICAL == "Yes" ~ 1, T ~ 0),
    imported.chem.facility = case_when(X90..IMPORT.THE.CHEMICAL == "Yes" ~ 1, T ~ 0),
    pi.chem.facility = case_when(
      # produced or imported for use, sales distribution, as a: by-product, manufactured impurity, reactant
      # and intermediate uses
      X91..ON.SITE.USE.OF.THE.CHEMICAL == "Yes" |
        X92..SALE.OR.DISTRIBUTION.OF.THE.CHEMICAL == "Yes" |
        X92..SALE.OR.DISTRIBUTION.OF.THE.CHEMICAL == "Yes" |
        X94..AS.A.MANUFACTURED.IMPURITY == "Yes" |
        X95..USED.AS.A.REACTANT == "Yes" ~ 1, T ~ 0),
    chemical.intermediate.uses = case_when(
      # chemical was used in chemical reactions to produce another chemical as: feedstocks, raw materials,
      # intermediates, initiators and others
      X96..P101..FEEDSTOCKS == "Yes" |
        X97..P102..RAW.MATERIALS == "Yes" |
        X98..P103..INTERMEDIATES == "Yes" |
        X99..P104..INITIATORS == "Yes" |
        X100..P199..OTHER == "Yes" ~ 1, T ~ 0),
    chemical.formulation.component = case_when(
      # chemical was added to a product mixture to improve product performance. It's added in the following forms:
      # additives, dyes, reaction diluents, initiators, solvents, inhibitors, emulsifiers, surfactants, lubricants,
      # flame retardants, rheological modifiers and others
      X101..ADDED.AS.A.FORMULATION.COMPONENT == "Yes" |
        X102..P201..ADDITIVES == "Yes" |
        X103..P202..DYES == "Yes" |
        X104..P203..REACTION.DILUENTS == "Yes" |
        X105..P204..INITIATORS == "Yes" |
        X106..P205..SOLVENTS == "Yes" |
        X107..P206..INHIBITORS == "Yes" |
        X108..P207..EMULSIFIERS == "Yes" |
        X109..P208..SURFACTANTS == "Yes" |
        X110..P209..LUBRICANTS == "Yes" |
        X111..P210..FLAME.RETARDANTS == "Yes" |
        X112..P211..RHEOLOGICAL.MODIFIERS == "Yes" |
        X113..P299..OTHER == "Yes" ~ 1, T ~ 0),
    chemical.article.component = case_when(
      # whether the facility uses the reported chemical as an integral component of an article distributed for
      # industrial, trade, or consumer use. They include repackaging, process impurity, process recycling,
      # processing aid, processing solvents, catalysts, inhibitors, initiators, reaction terminators, solution buffers
      # and others
      X114..USED.AS.AN.ARTICLE.COMPONENT == "Yes" |
        X115..REPACKAGING == "Yes" |
        X116..AS.A.PROCESS.IMPURITY == "Yes" |
        X117..PROCESSED...RECYCLING == "Yes" |
        X118..USED.AS.A.CHEMICAL.PROCESSING.AID == "Yes" |
        X119..Z101..PROCESS.SOLVENTS == "Yes" |
        X120..Z102..CATALYSTS == "Yes" |
        X121..Z103..INHIBITORS == "Yes" |
        X122..Z104..INITIATORS == "Yes" |
        X123..Z105..REACTION.TERMINATORS == "Yes" |
        X124..Z106..SOLUTION.BUFFERS == "Yes" |
        X125..Z199..OTHER == "Yes" ~ 1, T ~ 0),
    chemical.manufacturing.aid = case_when(
      # whether the chemical is used at this facility to  aid the manufacturing process, without intending for it to be
      # part of the resulting product or the reaction mixture, during the manufacture or synthesis of another
      # chemical substance. They include process lubricant, metalworking fluids, coolants, refrigerants, hyraulic fluids
      # and others
      X126..USED.AS.A.MANUFACTURING.AID == "Yes" |
        X127..Z201..PROCESS.LUBRICANTS == "Yes" |
        X128..Z202..METALWORKING.FLUIDS == "Yes" |
        X129..Z203..COOLANTS == "Yes" |
        X130..Z204..REFRIGERANTS == "Yes" |
        X131..Z205..HYDRAULIC.FLUIDS == "Yes" |
        X132..Z299..OTHER == "Yes" ~ 1, T ~ 0),
    chemical.ancilliary.use = case_when(
      # whether the chemical is used at this facility for
      # purposes other than aiding chemical processing or
      # manufacturing. Includes, but not limited to, cleaners,
      # degreasers, lubricants, fuels, and chemicals used for treating
      # wastes.
      X133..ANCILLARY.OR.OTHER.USE == "Yes" |
        X134..Z301..CLEANER == "Yes" |
        X135..Z302..DEGREASER == "Yes" |
        X136..Z303..LUBRICANT == "Yes" |
        X137..Z304..FUEL == "Yes" |
        X138..Z305..FLAME.RETARDANT == "Yes" |
        X139..Z306..WASTE.TREATMENT == "Yes" |
        X140..Z307..WATER.TREATMENT == "Yes" |
        X141..Z308..CONSTRUCTION.MATERIALS == "Yes" |
        X142..Z399..OTHER == "Yes" ~ 1, T ~ 0),
  ) %>%
  select(c(pid, year:industry.name, industry.category, chemical.id:total.underground.injection.onsite,
           total.landfills.onsite, total.releases.toland.treatment.onsite,
           total.surface.impoundment.onsite:total.underground.injection.offsite,
           total.landfills.offsite:total.releases.storage.offsite, total.releases.metalsolidify.offsite,
           total.wastewater.releases.npotw.offsite, total.releases.other.mgt.offsite,
           total.releases.wastebroker.offsite:chemical.ancilliary.use))

#===============================================================#
### Generating naics sector codes column.
#===============================================================#
tri$naics.sector.code[grepl(pattern = "^11", tri$naics.code)] <- "11"
tri$naics.sector.code[grepl(pattern = "^21", tri$naics.code)] <- "21"
tri$naics.sector.code[grepl(pattern = "^22", tri$naics.code)] <- "22"
tri$naics.sector.code[grepl(pattern = "^23", tri$naics.code)] <- "23"
tri$naics.sector.code[grepl(pattern = "^31", tri$naics.code)] <- "31-33"
tri$naics.sector.code[grepl(pattern = "^32", tri$naics.code)] <- "31-33"
tri$naics.sector.code[grepl(pattern = "^33", tri$naics.code)] <- "31-33"
tri$naics.sector.code[grepl(pattern = "^42", tri$naics.code)] <- "42"
tri$naics.sector.code[grepl(pattern = "^44", tri$naics.code)] <- "44-45"
tri$naics.sector.code[grepl(pattern = "^45", tri$naics.code)] <- "44-45"
tri$naics.sector.code[grepl(pattern = "^48", tri$naics.code)] <- "48-49"
tri$naics.sector.code[grepl(pattern = "^49", tri$naics.code)] <- "48-49"
tri$naics.sector.code[grepl(pattern = "^51", tri$naics.code)] <- "51"
tri$naics.sector.code[grepl(pattern = "^53", tri$naics.code)] <- "53"
tri$naics.sector.code[grepl(pattern = "^54", tri$naics.code)] <- "54"
tri$naics.sector.code[grepl(pattern = "^55", tri$naics.code)] <- "55"
tri$naics.sector.code[grepl(pattern = "^56", tri$naics.code)] <- "56"
tri$naics.sector.code[grepl(pattern = "^61", tri$naics.code)] <- "61"
tri$naics.sector.code[grepl(pattern = "^62", tri$naics.code)] <- "62"
tri$naics.sector.code[grepl(pattern = "^71", tri$naics.code)] <- "71"
tri$naics.sector.code[grepl(pattern = "^81", tri$naics.code)] <- "81"
tri$naics.sector.code[grepl(pattern = "^92", tri$naics.code)] <- "92"

sum_up(tri, c(energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite, industrial.boiler.onsite,
                recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
                biological.treatment.onsite, chemical.treatment.onsite, incineration.thermal.treatment.onsite,
                physical.treatment.onsite, material.subandmod, treatment.onsite, air.emissions.treatment.onsite,
                total.waste.management.onsite, sub.fuel.matsubmod, sub.organic.solvent.matsubmod,
                sub.rawm.feedstock.reactchem.matsubmod, sub.manu.proccess.ancilliary.chems.matsubmod,
                mod.content.grade.purity.chems.matsubmod, other.matmods.matsubmod, product.modification,
                devd.newproductline.pmod, alt.dim.comp.design.pmod, mod.packaging.pmod, other.pmods.pmod,
                process.equip.modification, optimised.process.efficiency.pequipmod, recirculationinprocess.pequipmod,
                newtech.technique.process.pequipmod, equipment.upgrade.update.pequipmod, other.pequipmods.pequipmod,
                inventory.material.mgt, better.labelling.testing.immgt, containers.sizechange.immgt,
                improved.materialhandling.operations.immgt, improved.monitoring.immgt, other.immgts.immgt,
                operating.practices.training, improved.schdule.operation.procedures.opt,
                changed.production.schedule.opt, intro.inline.productquality.process.analysis.opt))

write_rds(x = tri, file = "./Data_PhD/US/EPA/AQS/toxic_release_inventory/triR.rds", compress = "xz")