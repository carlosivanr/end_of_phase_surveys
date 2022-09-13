


library(rstatix)

# Correlation between overall clerkship evaluations and clerkship grades
eop3_grades %>%
  filter(EvalCC3_1 != 9) %>%
  cor_test(
    EvalCC3_1, 
    CRSE_GRADE_OFF, 
    method = "kendall"
    )

# Means
eop3_grades %>%
  filter(EvalCC3_1 != 9) %>%
  group_by(LIC) %>%
  summarise(mean_eval = mean(EvalCC3_1, na.rm = TRUE),
            mean_grade = mean(CRSE_GRADE_OFF, na.rm = TRUE))


# Correlation between clerkship evals and clerkship grades by LIC
# Evidence suggest that there is a significant relationship between grades and
# evals in those that took a traditional clerkship. In contrast, among those
# that took an LIC, the relationship between evals and grades was not signicant.
# This would indicate that it's possible to provide a high eval, but the grades
# may be different
eop3_grades %>%
  filter(EvalCC3_1 != 9) %>%
  group_by(LIC) %>%
  cor_test(
    EvalCC3_1, 
    CRSE_GRADE_OFF, 
    method = "kendall"
    )

