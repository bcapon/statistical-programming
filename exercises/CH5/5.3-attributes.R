data("PlantGrowth")
attributes(PlantGrowth)

A <- matrix(1:6, 2, 3)
attr(A, "dim")
attr(A, "foo") <- list(1:3, "fred")
attributes(A)