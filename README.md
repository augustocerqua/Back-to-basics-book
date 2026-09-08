# The Backbone of Impact Evaluation Methods – Back to Basics

This is the repository for the book "The Backbone of Impact Evaluation Methods – Back to Basics". The book can be accessed for free here: https://augustocerqua.github.io/Back-to-basics-book/

## About the book

This book is about impact evaluation and causal inference. Policies, interventions and shocks shape many areas of economic and social life, yet observing what happened after an intervention is not enough to know what the intervention caused. To estimate an impact, we need to compare observed outcomes with a credible counterfactual: what would have happened in the absence of the treatment. This book revisits the backbone of the most widely used impact evaluation methods, starting from the potential outcomes framework and the definition of causal parameters, and then moving to randomized experiments, selection-on-observables methods, difference-in-differences, event studies, instrumental variables, regression discontinuity designs, synthetic control methods, and other alternative approaches. The goal is not to provide an exhaustive technical treatment, but to make the logic, assumptions, strengths, and limitations of each method as transparent as possible. This book is intended for students, researchers, policy analysts, and practitioners who want to understand when causal claims are credible, what assumptions support them, and how empirical strategies can be used to inform better policy decisions.

## Rendering and publishing

Render the website with `quarto render --to html`. The complete HTML site is
written to `docs`; commit and push this directory to update GitHub Pages.

Render the PDF with `quarto render --profile pdf --to pdf`. This profile writes
to `_book-pdf`, keeping the published HTML site separate. Do not run
`quarto render --to pdf` without the profile: it would use the website output
directory. Before publishing, check that `docs/index.html` contains the book
homepage, not a redirect to itself.

## This version

This is version 0.3 of the book. It is still a work in progress, so some errors, imprecisions, and unfinished parts may remain.
