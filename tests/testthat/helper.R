eq_with_class = function(x, y){
    if (is.list(y)) {
        if (length(y)) {
            for ( i in 1:length(y)) {
                class(y[[i]]) = 're2_matrix'
            }
        }
    } else {
        class(y) = 're2_matrix'
    }
    expect_equivalent(x,y)
}
