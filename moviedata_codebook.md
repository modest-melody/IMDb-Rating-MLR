# Codebook for Movies Dataset

## Data Description

The dataset comprises 651 randomly sampled movies that were produced and released before the year 2016.

Some variables in this dataset are included for informational purposes and may not be suitable for statistical analysis. It's important to discern which variables are meaningful and which can be omitted. For instance, the information in `actor1` through `actor5` was used to determine if the movie casts an actor or actress who has won a best actor or actress Oscar.

Consider omitting certain observations or restructuring some variables to ensure they are suitable for answering your research questions. When fitting a model, be cautious of collinearity, as some variables might be dependent on each other.

## Codebook

- `title`: Title of the movie.
- `title_type`: Type of movie. Possible values are Documentary, Feature Film, and TV Movie.
- `genre`: Genre of the movie. Possible values include Action & Adventure, Comedy, Documentary, Drama, Horror, Mystery & Suspense, and Other.
- `runtime`: Runtime of the movie in minutes.
- `mpaa_rating`: MPAA rating of the movie. Possible values are G, PG, PG-13, R, and Unrated.
- `studio`: Studio that produced the movie.
- `thtr_rel_year`: Year the movie was released in theaters.
- `thtr_rel_month`: Month the movie was released in theaters.
- `thtr_rel_day`: Day of the month the movie was released in theaters.
- `dvd_rel_year`: Year the movie was released on DVD.
- `dvd_rel_month`: Month the movie was released on DVD.
- `dvd_rel_day`: Day of the month the movie was released on DVD.
- `imdb_rating`: Rating of the movie on IMDB.
- `imdb_num_votes`: Number of votes the movie received on IMDB.
- `critics_rating`: Categorical variable for critics rating on Rotten Tomatoes. Possible values are Certified Fresh, Fresh, and Rotten.
- `critics_score`: Critics score on Rotten Tomatoes.
- `audience_rating`: Categorical variable for audience rating on Rotten Tomatoes. Possible values are Spilled and Upright.
- `audience_score`: Audience score on Rotten Tomatoes.
- `best_pic_nom`: Whether or not the movie was nominated for a Best Picture Oscar. Possible values are no and yes.
- `best_pic_win`: Whether or not the movie won a Best Picture Oscar. Possible values are no and yes.
- `best_actor_win`: Whether or not one of the main actors in the movie has won an Oscar (not necessarily for this movie). Possible values are no and yes.
- `best_actress_win`: Whether or not one of the main actresses in the movie has won an Oscar (not necessarily for this movie). Possible values are no and yes.
- `best_dir_win`: Whether or not the director of the movie has won an Oscar (not necessarily for this movie). Possible values are no and yes.
- `top200_box`: Whether or not the movie is in the Top 200 Box Office list on BoxOfficeMojo. Possible values are no and yes.
- `director`: Director of the movie.
- `actor1`: First main actor/actress in the abridged cast of the movie.
- `actor2`: Second main actor/actress in the abridged cast of the movie.
- `actor3`: Third main actor/actress in the abridged cast of the movie.
- `actor4`: Fourth main actor/actress in the abridged cast of the movie.
- `actor5`: Fifth main actor/actress in the abridged
