
#  pip3 install git+https://github.com/alberanid/imdbpy
from imdb import IMDb
import csv

# create an instance of the IMDb class
ia = IMDb()

top250 = ia.get_top250_movies()

with open('movies.csv', 'w') as csvfile:
    fieldnames = ['id', 'name', 'v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9', 'v10', "year", "rating"]
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    for m in top250:
        movie = ia.get_movie(m.movieID)
        ia.update(movie, ['main', 'vote details'])
        votes = movie.data["number of votes"]
        title = str(movie.data["title"]).replace(',', ' ')
        writer.writerow({'id': movie.movieID,
                         'name': title,
                         'v1': votes[1],
                         'v2': votes[2],
                         'v3': votes[3],
                         'v4': votes[4],
                         'v5': votes[5],
                         'v6': votes[6],
                         'v7': votes[7],
                         'v8': votes[8],
                         'v9': votes[9],
                         'v10': votes[10],
                         'year': movie.data["year"],
                         'rating': movie.data["rating"]
                         })


