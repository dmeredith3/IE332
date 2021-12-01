library(cronR)

cron_ls()
path = "/Users/dmeredith/Documents/GitHub/IE332/Daily update.R"
cmd = cron_rscript(path)
cron_add(command= cmd, frequency = 'daily', at="00:00", days_of_week = c(0:6),
         id = 'DailyUpdate', description = 'DailyStatsUpdate')