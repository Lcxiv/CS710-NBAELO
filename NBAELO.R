team_elo <- nbaallelo$elo_i
opp_elo <- nbaallelo$opp_elo_i
team_pts <- nbaallelo$pts
opp_pts <- nbaallelo$opp_pts

game_r <- nbaallelo$game_result

plot(opp_elo,opp_pts)
plot(team_elo,opp_elo)


a <-ggplot(nbaallelo,aes(x=elo_i,y=pts))
a + geom_line()

result_table <- sqldf("select game_id,fran_id,opp_fran,elo_i,opp_elo_i,pts,opp_pts,game_result 
                      from nbaallelo 
                      where elo_i > opp_elo_i AND pts > opp_pts
                      order by fran_id")
result_table$pts_diff <- result_table$pts - result_table$opp_pts
result_table$pts_avg_per_team <- apply(result_table, 1, function(x) { mean(x, na.rm=TRUE) })


View(result_table)


ggplot(result_table, aes(pts_diff, fran_id)) + geom_boxplot() + coord_cartesian(xlim = c(0, 60), ylim = c(0, 30)) + 
  labs( x = "Difference in Points ", y = "New y axis label",
          title ="Add a title above the plot")

pts_avg_per_team  <- aggregate(x = result_table$pts_diff,                # Specify data column
         by = list(result_table$fran_id),              # Specify group indicator
         FUN = mean)
ggplot(pts_avg_per_team, aes(x,Group.1)) + geom_point() + coord_cartesian(xlim = c(0, 20)) + 
  labs( x = "Difference on average in Points ", y = "Teams",
        title ="Average points scored by teams who won with a higher elo than their opponent")

ggplot(pts_avg_per_team, aes(x,Group.1)) + geom_col() +   
  labs( x = "Difference on average in points per game ", y = "Teams",
  title ="Average points scored by teams who won with a higher elo than their opponent")
