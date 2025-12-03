

top = 25
num_dim = 2

dist_to_base <- function (base, x1, x2) 
    {
    sqrt ((x1 - base)^2
          + (x2 - base)^2
          )
    }

cat ("\n")

center = median (1:top)

num_entries = top^num_dim

d2 = data.frame (Instances = 1:num_entries, 
                    Source = rep("gridTest"), 
                    feature_x1 = numeric(num_entries), 
                    feature_x2 = numeric(num_entries), 
                    algo_dist_to_base = numeric(num_entries)
                    )
                    
#base = median(1:top)
base = center

cat ("Instances, Source"
     , ", feature_x1"
     , ", feature_x2"
     , ", algo_dist_to_base")

ct = 0
for (x1_idx in 1:top)
{
for (x2_idx in 1:top)
{
            ct = ct + 1
            d2[ct,3] = x1_idx
            d2[ct,4] = x2_idx
            d2[ct,5] = round (dist_to_base (base
                                            , x1_idx
                                            , x2_idx
                                            )
                                            , 2)
            
            cat ("\n", ct, 
                 
                 ", ", x1_idx, 
                 ", ", x2_idx, 

                 ", ", round (dist_to_base (base 
                                            , x1_idx
                                            , x2_idx
                                            )
                                            , 2)
                 , sep='')
}
}

cat ("\n")

write_csv (d2, "~/Downloads/matildaRegularDist2_toCenter.csv")

#  Started at 1:24
