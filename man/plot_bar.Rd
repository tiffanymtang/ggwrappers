% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base-plots.R
\name{plot_bar}
\alias{plot_bar}
\title{Bar plot wrapper for ggplot2}
\usage{
plot_bar(
  data,
  x_str,
  y_str = NULL,
  fill_str = NULL,
  fill = "#6FBBE3",
  stat = "count",
  position = "dodge",
  ...
)
}
\arguments{
\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{x_str}{Character string. Name of variable in \code{data} to plot on
x-axis.}

\item{y_str}{Character string. Name of variable in \code{data} to plot on
y-axis. Used only if \code{stat = "identity"}.}

\item{fill_str}{Character string. Name of variable in \code{data}
to use as fill aesthetic in plot.}

\item{fill}{Fill color. Only used if \code{fill_str} is \code{NULL}.}

\item{stat}{See \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}.}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments to pass to \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}.}
}
\value{
A ggplot bar plot.
}
\description{
Convenient wrapper function around \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}.
}
\examples{
df <- data.frame(x = rep(letters[1:3], 2), y = rep(LETTERS[1:2], 3))
plot_bar(data = df, x_str = "x")
plot_bar(df, x_str = "x", fill_str = "y")

}
