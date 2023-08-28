# # # # # # # # # # # #
#
# MODULE 3 SCRIPT
#
# # # # # # # # # # # #
# 21.08.23 by Noelle
# Summary of the commands learned in Module 3

library(ggplot2)
library(dplyr)


# # # # # #
# 4- R4DS CHAPTER 4 ----
#
library(nycflights13)
flights


# # #
# 4.2. rows ----
#
# 4.2.1 filter() ----
filter(flights, dep_delay > 60)
flights |> filter(dep_delay > 60)
flights %>% filter(dep_delay > 60)

jan1 <- flights |> filter(month == 1 & day == 1)
flights |> filter(month == 1 | month == 2)
flights |> filter(month %in% c(1, 2))

# what's happening if "|" not used correctly?
#  any number is treated as TRUE because it's not 0 (FALSE)
a <- data.frame(a = c("A", "B", "B", "C"), b = c(5, 4, 2, 1))
a |> filter(b == 1 | 5)
a |> filter(b == 1 | 0) # only one row is filtered

#
# 4.2.3 arrange() ----
# row ordering
flights |> arrange(year, month, day, dep_time)
flights |> arrange(year, desc(month), day, dep_time) # descending order
# unmentioned columns are left in original order :
a <- data.frame(a = c("A", "B", "B", "C"), b = c(5, 4, 2, 1))
a |> arrange(desc(a))

#
# 4.2.4 distinct() ---
distinct(flights)
distinct(flights, origin, dest)
distinct(flights, origin, dest, .keep_all = T)

# count number of occurrences (of unique cobinations)
testdat1 <- tibble(a = c("A", "B", "B", "B", "C"), 
                   b = c(5, 4, 4, 2, 1),
                   c = c(12, 2, 3, 34, 12))
count(testdat1, a, b, sort = T)
count(testdat1, a)

# exercises
flights |> filter(dep_delay > 2 & 
                    dest %in% c("HOU", "IAH") &
                    carrier %in% c("UA", "DL"))


# # #
# 4.3. columns ----
#
# 4.3.1 mutate() ----
flights |> mutate(
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60
 ) |>
  select(gain, speed)
flights |> mutate(
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60,
  .before = 1
)
flights |> mutate(
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60,
  .after = year
)
flights |> mutate(
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60,
  .after = year,
  .keep = "used"
)

# 4.3.1 select() ----
flights |> 
  select(year, month, day)

flights |> 
  select(year:day)
flights |> 
  select(!year:day)
flights |> 
  select(-(year:day))

flights |> 
  select(where(is.character))

# renaming variables
flights |> 
  select(newname = tailnum, everything())

# 4.3.3 rename() ----
flights |> rename(tail_num_new = tailnum)

# 4.3.3 relocate() ----
flights |> relocate(day, arr_time)
flights |> relocate(day, .after = year)


# 4.5.1 group_by() ----
flights |> 
  group_by(month) |> 
  summarize()

# 4.5.1 group_by() with summarize() ----
flights |> 
  group_by(month) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = T),
            number = length(dep_delay),
            n = n()) |> 
  mutate(res = n - number) |> 
  select(res) |> 
  as.vector()

# 4.5.1 group_by() with slice_ ----
flights |> slice_head(n = 1)
flights |> group_by(month) |>  slice_head(n = 1)
flights |> group_by(month) |>  slice_min(dep_time, n = 1)
flights |>  slice_sample(n = 5)
flights |>  slice_sample(prop = 0.1)

# 4.5.4 group_by() with multiple variables
flights_small <- flights |> 
  filter(origin == "JFK",
         carrier == "AA",
         sched_arr_time > 2000,
         dep_delay < -10)

daily <- flights_small |>  
  group_by(year, month, day)
daily |> summarize(n = n()) # only two groups left
daily |> summarize(n = n(),
                   .groups = "drop_last")
daily |> summarize(n = n(),
                   .groups = "keep")

daily |> 
  ungroup()

daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )

# 4.5.4 .by as alternative to group_by()
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )







# # # # # #
# BECKERMAN CHAPTER 3 ----
#
# # #
# 3.1
compensation <- read.csv("Introduction_to_R_Summer2023/compensation.csv")
summary(compensation)
names(compensation)
dim(compensation)
head(compensation)
str(compensation)
tbl_df(compensation)
glimpse(compensation)
