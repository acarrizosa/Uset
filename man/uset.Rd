\name{uset}
\alias{uset}
\title{Uncovered Set Calculation}

\description{
A simple function to estimate the uncovered set and yolk of a set of two-dimensional ideal points.
}
\usage{
uset(dat, uset.coarse = TRUE, uset.fine = FALSE, uint.coarse = 0.1,
        uint.fine = 0.02, threshold = 0.5, yolk = FALSE, yolkint.coarse = 0.05,
        yolkint.fine = 0.01, points.on.line = 1000, plotuset = TRUE,
        clusters = "default")
}
\arguments{

\item{dat}{is simply the inputed data.  It must be a data.frame object, and the first two columns must be named "x" and "y". The input data may include more columns than two, which may be named however one prefers, but if the first two columns do not meet these conditions the function returns an error.}

\item{uset.coarse}{}
\item{dat}{is TRUE if one would like to carry out the estimation of the coarse uset.  If FALSE, the function will either immediately carry out the `uset.fine` search, or it will immediately search for the yolk.  I do not recommed `uset.coarse` to ever be FALSE because it can make the calculation time a lot longer.}
\item{uset.fine}{asks whether the fine search for the uncovered set should be carried out.}
\item{uint.coarse}{specifies the interval of the coarse gridsearch.  By default this coarse grid interval is of .1}
\item{uint.fine}{this creates the interval for the fine search.  By default the fine grid interval is of .02}
\item{threshold}{specifies the threshold necessary for a bill to pass.  By default the threshold is set at .5 assuming that bills have an absolute majority threshold.  The threshold must be surpassed, and not simply met so that the function actually captures the 50\%+1 votes necessary to pass a bill.  That being said, higher thresholds can also be considered such as 3/5 or 2/3 by modifying this argument.  With higher threhsolds the uncovered set may coincide with the core, should a core exist.}
\item{yolk}{if TRUE the function also searches for the yolk.  By default it is set to FALSE.}
\item{yolkint.coarse}{sets the interval for the coarse grid used to search for the yolk.}
\item{yolkint.fine}{sets the interval for the fine grid used to search for the yolk.}
\item{points.on.line}{to search for the yolk median lines have to be calculated, and distances have to be taken from chosen yolk candidates to each of these median lines.  This option specifies how many points to place on each median line to take the distances of these points to each yolk point candidate.  The chosen yolk center becomes the point that can minimize the distance from the furthest of these median lines.  This is equivalent to saying the center of the yolk creates the smallest circle possible that minimizes the distance from all median lines.  By default 1,000 points are placed on each median line.  Smaller numbers may be chosen to make estimation faster, but I have found 1,000 to be the best option for most purposes.  Making the number larger makes estimation time a lot longer, and making the number smaller can lead to non-intuitive yolk centers being chosen.}
\item{plotuset}{specifies whether the `uset` function should return a plot or not.  If TRUE, `uset` returns a ggplot object plotting ideal points, median lines, the uncovered set, the yolk, and the 4r circle, depending on which of these were requested from the `uset` function in previous arguments.}

\item{clusters}{determines the number of cores to use for estimation.  This can be specified as either "default", or "optimal", or it can be assigned a numeric value. "default" runs the function using a single core.  "optimal" takes the number of cores of your computer, minus one, and assigns it to the cluseters variable.  Finally, one may also assign a number to this option, such as `clusters=5`.  If a number is chosen, the number must be smaller than the total number of cores of the computer trying to run the function.  Otherwise the function stops and returns an error.}
}

\value{

\item{dat}{the function stores and returns the data you provided it.}
\item{yolkcenter}{this gives the location of the center of the yolk as "x" and "y" coordinates.}
\item{yolkradius}{this gives the radius of the yolk.}
\item{yolkdiameter}{this gives the diameter of the yolk.}
\item{medianlines}{this provides all the median lines.  These lines are provided as slopes (b) and intercepts (m), but they are also provided as connection between points--(x1,y1) to (x2,y2).}
\item{uset}{This provides all the points that were found to be part of the uncovered set.}

If plotuset is TRUE, the function also provides a ggplot object as the seventh item in the output list.

}

\details{
Data must be submitted to the function in a data frame form,
and the first two columns of the data frame must be named "x" and "y respectively.  The function carries out a grid search procedure to find the uncovered set and yolk.

First a coarse search is carried out to find the general contours of the uncovered set.  Then a fine search is carried out to narrow down the uncovered set.  Once the uncovered set is calculated (whether the fine search is carried out or not is optional), this contour of the uncovered set is used to search for the yolk.

Because the center of the yolk is contained by the uncovered set, the center of the yolk is only searched for within a square that contains the uncovered set.  Median lines are calculated, and, by default, 1,000 points are placed on all median lines.  From this, each point in the uncovered-set-defined square is used to take the distance from each point on the median lines.  Then, we take the smallest distance from each line.  Then we find, from these smallest distances, the largest of the smallest distances.  That is, I find the median line that is furthest away from the point.  The point that minimizes the furthest distance in the uncovered-set-defined square is chosen as the center of the yolk.

The radius of the yolk becomes the distance of this chosen point to the furthest line.  When plotting, this function returns the ideal points, median lines, the uncovered set, (whether coarse or fine), the center of the yolk, the yolk, and the 4r circle.

The function itself returns all these data as well, including the radius of the yolk, in a list.

To a great extent this procedure has been based out on the grid search procedure developed by:

Bianco, William, Ivan Jeliazkov and Itai Sened (2004) "The Uncovered Set and the Limits of Legislative Action" Political Analysis 12:256-276

Please cite this package as:

Carrizosa, Andres (2020). uset: a simple function to extract the uncovered set and yolk from two-dimensional ideal points. URL: www.github.com/acarrizosa/uset
}

\examples{
data(pydata)

output<-uset(pydat,
        uint.coarse=.1,
        yolk=T,
        yolkint.coarse=.1,
        yolkint.fine=0.05,
        plotuset=T,
        points.on.line = 1000,
        clusters="optimal")

output
}
