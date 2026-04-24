This paper presents an application of Conformal Prediction to a classical credit-risk classification problem
using the German Credit dataset and two base classifiers: Naive Bayes and Random Forest. Using split conformal
prediction, we construct prediction sets for the risk label with a marginal coverage guarantee at the nominal
90% level under the assumption of data exchangeability. We assess performance through empirical coverage
and average set size on a held-out test set, in addition to standard metrics of the base models (accuracy and
AUC). The results show that empirical coverage remains close to or above the nominal level, while the average
set size indicates a substantial proportion of clear decisions (|ˆ
C (x)| = 1) and a smaller fraction in the gray zone
(|ˆ
C (x)| = 2). We discuss how this decision-zone decomposition can support business rules by distinguishing
approvals and rejections with greater confidence and routing ambiguous cases to additional review. Finally, we
outline future directions involving more advanced variants of Conformal Prediction and evaluation criteria
that account for asymmetric error costs.
