

top = 5
num_dim = 4

dist_to_base <- function (base, x1, x2, x3, x4) 
    {
    sqrt ((x1 - base)^2
          + (x2 - base)^2
          + (x3 - base)^2
          + (x4 - base)^2
          )
    }

cat ("\n")

center = median (1:top)

num_entries = top^num_dim

d4 = data.frame (Instances = 1:num_entries, 
                    Source = rep("gridTest"), 
                    feature_x1 = numeric(num_entries), 
                    feature_x2 = numeric(num_entries), 
                    feature_x3 = numeric(num_entries), 
                    feature_x4 = numeric(num_entries), 
                    algo_dist_to_base = numeric(num_entries)
                    )
                    
#base = median(1:top)
#base = 0
base = center

cat ("Instances, Source"
     , ", feature_x1"
     , ", feature_x2"
     , ", feature_x3"
     , ", feature_x4"
     , ", algo_dist_to_base")

ct = 0
for (x1_idx in 1:top)
{
for (x2_idx in 1:top)
{
for (x3_idx in 1:top)
{
for (x4_idx in 1:top)
{
            ct = ct + 1
            d4[ct,3] = x1_idx
            d4[ct,4] = x2_idx
            d4[ct,5] = x3_idx
            d4[ct,6] = x4_idx
            d4[ct,7] = round (dist_to_base (base
                                            , x1_idx
                                            , x2_idx
                                            , x3_idx
                                            , x4_idx
                                            )
                                            , 2)
            
            cat ("\n", ct, 
                 
                 ", ", x1_idx, 
                 ", ", x2_idx, 
                 ", ", x3_idx, 
                 ", ", x4_idx, 

                 ", ", round (dist_to_base (base 
                                            , x1_idx
                                            , x2_idx
                                            , x3_idx
                                            , x4_idx
                                            )
                                            , 2)
                 , sep='')
}
}
}
}

cat ("\n")

hist(d4$algo_dist_to_base)
     
write_csv (d4, "~/Downloads/matildaRegularDist4_toCenter.csv")

#  Started at 1:46
#  This failed one minute later because it said that all features were 
#  correlated and so it only wanted to keep 1 feature, which isn't enough 
#  for the projection onto 2D.
#  Not sure why this example was too correlated but the others weren't.
#  Here's the end of the log file:
#  
#  =========================================================================
# -> Calling SIFTED for auto-feature selection.
# =========================================================================
# -> Selecting features based on correlation with performance.
# -> Keeping 1 out of 4 features (correlation).
# EOF:ERROR
# {Error using SIFTED (line 55)
# -> There is only 1 feature. Stopping space construction.
# 
# Error in buildIS (line 303)
# [model.data.X, model.sifted] = SIFTED(model.data.X, model.data.Y,
# model.data.Ybin, opts.sifted);
# 
# Error in exampleWeb (line 4)
# model = buildIS(rootdir);
# } 
