

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
base = 0

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

write_csv (d4, "~/Downloads/matildaRegularDist4.csv")


