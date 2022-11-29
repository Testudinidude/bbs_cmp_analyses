install.packages('rsconnect')

rsconnect::setAccountInfo(name='testudinidude',
                          token='EA1FCC8A48D938414F3A9795AADF74E2',
                          secret='hfCc/+PVGa/BxCiV8tJMS1y43z5zFYMeisvmlqp/')
library(rsconnect)
rsconnect::deployApp('G:/NaturalResources/Byer/DataAnalysis/bbs_cmp_analyses/bbsapp')



