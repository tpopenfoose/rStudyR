

TEST <- muffin.ML$new('EURUSD', 'M30', '.')
TEST$input.price.data(EURUSD_M30_PRICE)
xx <- TEST$build.model()

# TEST$get('best.factors')
# xx[, OUTPUT] %>% table %>% print