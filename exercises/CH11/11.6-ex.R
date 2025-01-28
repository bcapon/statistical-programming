## 11.6 (X^TX)^{-1}^Ty = (VD^2V^T)^{-1}VDU^Ty=VD^{-2}V^TVDU^Ty = VD^{-1}U^Ty
X <- model.matrix(~group,PlantGrowth)
sx <- svd(X)
beta.hat <- sx$v%*%((t(sx$u)%*%PlantGrowth$weight)/sx$d)
beta.hat;coef(lm(weight~group,PlantGrowth))
