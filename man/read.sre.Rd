\name{read.sre}
\alias{read.sre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to read in an SRE data structure from file }
\description{
  This function reads a submission to a NIST-type Speaker Recognition
  Evaluation from file into an \code{sre} data frame.  For supported
  evaluations, currently NIST 2008 and EVALITA 2009, it attempts to add
  target information to the data frame.  It blesses the
  \code{data.frame} to be of class \code{sre}.
}
\usage{
read.sre(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A file containing trials in NIST submission format}
}
\details{
  This function attempts to recognize the actual evaluation from the
  content.  It does this by looking at the model IDs.  It is important
  that these are the same as in the key \code{sretools:::srekey}.  Then
  it adds the key information, i.e., whether each trial is a target or a
  non-taget trial in a colum \code{target}.  Metadata in the trial list
  is read into columns, and additional metadata from the key is added.
  This metadata may vary per evaluation.

  In a Unix environment, you can specify a command as \code{file} ending
  in a pipe symbol \code{|}, in which case the command is executed and
  the stdout of that command is used as input.  This will allow for
  compressed files, concatenation of files and filtering. 

  An alternative to this function is \code{read.tnt}, which just reads
  target and non-target scores from file. 
}
\value{%% no space allowed after \item!
  The function returns a \code{data.frame} of class \code{sre} which has
  a minimum of three fields, \code{score}, \code{dec} and
  \code{target}.  Common fields include \code{model}, \code{test},
  \code{gender}, \code{mcond}, \code{tcond}, \code{adapt}.  NIST SRE
  2008 fields are \code{chan}, \code{mlang}, \code{tlang},
  \code{\mtype}, \code{ttype}, \code{mmic} and \code{tmic}.  An EVALITA SRE
  2009 additional field is \code{channel}.
  
  \item{score}{The score of the detection trial}
  \item{dec}{The decision of the trials, either TRUE or FALSE}
  \item{target}{The truth about whether this was a target or
    non-target trial }
  \item{model}{Model (train) ID, or target speaker in the trial}
  \item{test}{Test segment ID of the trial (may need additional
    \code{chan})}
  \item{chan}{The channel (side) of the test segment, \code{a} or \code{b}}
  \item{gender}{Sex of target (model) speaker \code{f} or \code{m}}
  \item{mcond}{The model (train) condition of the trial}
  \item{tcond}{The test condition of the trial}
  \item{mlang}{The language of the model (train segment)}
  \item{tlang}{ The language of the test segment }
  \item{mtype}{ Recording type of model segment \code{interview} or
    \code{phonecall}}
  \item{ttype}{ Recording type of tyest segment}
  \item{mmic}{ Transducer type of model segment \code{mic} or
    \code{phn}}
  \item{tmic}{ Transsducer type of test segment}
  \item{channel}{The telephone channel of the test segment, PSTN
    \code{P} or GSM \code{G}}
}
\note{
  In SRE, we have a problem that both `train', `test' and `target' start
  with the same letter.  In \code{sretools} we therefore adopt the
  nomenclature to reference to `train' as `model' so that we can use the
  character `m'.  This may be a bit confusing.  NIST has adopted another
  standard, calling the `test' `segment' (as if the training does not
  come from a speech segment).
  
  The addition of that target and other metadata robably happens a bit
  clumsy.  Models and test segment/channel need to be in the case as
  they are in the key data.  This restriction may be relaxed in the
  future. 
}

%%\references{ ~put references to the literature/web site here ~ }
\author{David A. van Leeuwen}
\seealso{\code{read.tnt} for an alternative, \code{det.sre} computing
  performance statistics and preparing for plotting,
  \code{\link{srekey}} for the
  information known about past SREs.}
\examples{
## prepare a NIST submission file
# read.sre("mysubmission.txt")
## 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
