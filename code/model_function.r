## Author: Chunyi Zhao 
## Modeling functions 

fit.with.these.features = function(model_data, feature_lists){
    if (length(feature_lists)==0){
        print("No covaraites, use 1")
    }
    this_formula = if (length(feature_lists)==0) "match ~ 1" else paste("match ~", paste(feature_lists, collapse="+"))   
    
    M = glm(formula = this_formula, data = model_data, family = binomial(link="logit"))
    return(M)
    
}


extract.significant.feature = function(model, alpha=0.05){
    model_coef = summary(model)$coefficient
    covar = names(model_coef[, 4])
    sig_covar = covar[which(model_coef[, 4] <= alpha)]
    return(sig_covar)
}



clean.up.sigificant.feature = function(sig_feature, factor_features=FACTOR_FEATURES){
    feature_reduced = sig_feature[grep(paste(c(factor_features, "(Intercept)"), collapse = "|"), sig_feature, invert=T)]
    for (this_factor in factor_features){
        if (any(grepl(this_factor, sig_feature))){
            feature_reduced = c(feature_reduced, this_factor)
        }
    }
    return(feature_reduced)
}


auto.fit.and.model.compare = function(model_data, features, factor_features=FACTOR_FEATURES, alpha=0.05){
    
    M0 = fit.with.these.features(model_data, features)
    m0_sig_feature = extract.significant.feature(M0, alpha)
    feature_reduced = clean.up.sigificant.feature(m0_sig_feature, factor_features)
    no_sig_feature = length(feature_reduced)==0 
    if(no_sig_feature) print("no significant features")
    ## Use if else, don't use ifelse, ifelse requires the two to be the same class
    feature_reduced = if (no_sig_feature)features else feature_reduced
    
    M1 = fit.with.these.features(model_data, feature_reduced)
    
    F_compare = anova(M1, M0, test="LRT")
    return(list(m0_sig_features = m0_sig_feature, 
                m1_sig_features =extract.significant.feature(M1), 
                M0 = M0, 
                M1 = M1, 
                compare= F_compare))
}

auto.fit.and.step.select = function(model_data, features, factor_features=FACTOR_FEATURES, alpha=0.05){
    
    M_full = fit.with.these.features(model_data, features)
    M_null = glm(match ~ 1, data = model_data, family = binomial(link="logit"))
    M1 = step(M_null, scope=list(upper=M_full), direction="both", test="Chisq", data = model_data, trace=0)
    F_compare = anova(M1, M_full, test="LRT")
    return(list(m0_sig_features = extract.significant.feature(M_full), 
                m1_sig_features =extract.significant.feature(M1), 
                M0 = M_full, 
                M1 = M1, 
                compare= F_compare))
   
}


