

# Test the function
library(nlme)
library(lme4)

### Test Setup ###

#setup <- LMMsetup(score ~ Machine, Machines, list("Worker", c("Worker", "Machine")))
#print(setup)

### Default Test ###

result <- lmm(score ~ Machine, Machines, list("Worker", c("Worker", "Machine")))  
lmer_result <- lmer(score ~ Machine + (1 | Worker) + (1 | Worker:Machine),
                    data = Machines,
                    REML = FALSE
)
lmer_result
result

### Default Empty List Test ###

result <- lmm(score ~ Machine, Machines)  
lm_result <- lm(score ~ Machine,
                data = Machines)
summary(lm_result)
result

## Assay Tests
result <- lmm(logDens ~ dilut + sample + Block, Assay, list(c("Block","sample")))
result
lmer_result <- lmer(logDens ~ dilut + sample + Block + (1 | Block:sample),
                    data = Assay,
                    REML = FALSE
)
lmer_result

## Meat Tests ##

result <- lmm(score ~ Storage + Block, Meat, list(c("Block", "Storage")))
result
lmer(score ~ Storage + Block + (1 | Block) + (1 | Storage) + (1 | Block:Storage),
     data = Meat,
     REML = FALSE
)