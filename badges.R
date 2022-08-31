#' BARDS Software Development Lifecycle Badges
#'
#' These helpers produce the markdown text you need in your README to include badges
#' that classify properties of an R package or function
#'
#' @section Specification:
#' \if{latex}{
#'  Logic for creating \code{baamr_xxx_badge} function (describle your function's specification)
#'  \itemize{
#'  \item define badge name and purpose
#'  \item customize badge color with corresponding stage/purpose
#'  \item link to corresponding hyperlink/url}
#'  The followings are some possible code:
#'  \itemize{
#'  \item \code{library(mkstdpkg)}
#'  \item \code{baamr_risk_badge("open")}
#'  \item \code{baamr_sdlc_badge("specification")}
#'  \item \code{baamr_codecov_badge(0)}
#'  \item \code{baamr_build_badge("passing")}
#'  }
#'
#'
#'  Retrieve code coverage percentage
#'  (to be updated in baamr_codecov_badge function):
#'  \itemize{
#'  \item \code{x <- covr::package_coverage()}
#'  \item \code{covr::percent_coverage(x, by="file")}
#'  }
#'
#'  Report Parameters:
#'  \tabular{lllll}{
#'    \bold{Parameter}  \tab  \bold{Label} \tab \bold{Description} \tab \bold{Default} \tab \bold{Valid Values}\cr
#'    risk  \tab Package risk category \tab Indicate a package's risk \tab NA \tab  "open", "moderate", "high" \cr
#'     \tab \tab category in an R-SDLC badge \tab \tab \cr
#'    stage  \tab R-SDLC stage \tab Indicate a package's development \tab NA \tab "specification", "development", \cr
#'     \tab \tab stage in an R-SDLC badge  \tab \tab "validation", "customer-review",   \cr
#'     \tab \tab \tab \tab "stable", "deprecated", \cr
#'     \tab \tab \tab \tab "retired" \cr
#'    codecov  \tab Code coverage \tab Indicate a package's code \tab 0 \tab  0~100 \cr
#'     \tab \tab coverage degree  \tab \tab \cr
#'    build  \tab Build Status \tab Indicate a package's build status \tab NA \tab  "passing", "failing"
#'  }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#' @details
#' * `baamr_risk_badge()`: badge declares the risk category of a package,
#' according to ERxx:
#'   - Low
#'   - Moderate
#'   - Open
#' * `baamr_sdlc_badge()`: badge declares the developmental stage of a
#' package, according to ERxx:
#'   - Specification
#'   - Development
#'   - Validation
#'   - Customer Review
#'   - Stable
#'   - Deprecated
#'   - Retired
#' * `baamr_validation_badge()`: badge indicates the validation step of a
#' package, according to ERxx:
#'   - Independent Testing
#'   - Double Programming
#' * `baamr_codecov_badge()`: badge indicates the percentage of a package's code
#' which is covered by automated tests, available on \href{https://builds-staging.merck.com/}{Jenkins powered by Merck}.
#' * `baamr_build_badge()`: badge indicates the build status of a package
#' available on \href{https://builds-staging.merck.com/}{Jenkins powered by Merck}.
#' @param risk Risk category of the package.
#' @param stage Stage of the package development lifecycle.
#' @param validation Validation type of the function.
#' @param codecov Code coverage of the package.
#' @param build Build status of the package.
#' @name badges
#' @examples
#' \dontrun{
#' baamr_risk_badge("open")
#' baamr_sdlc_badge("specification")
#' baamr_codecov_badge(0)
#' baamr_build_badge("passing")
#' }
NULL

badges <- list(
  risk = c("low", "moderate", "open"),
  sdlc_stages = c(
    "specification", "development", "validation", "customer-review",
    "stable", "deprecated", "retired"
  ),
  validation = c("independent-testing", "double-programming"),
  build = c("failing", "passing"),
  doclink = "https://collaboration.merck.com/sites/RInitative/Shared%20Documents/Forms/AllItems.aspx?sortField=Modified&isAscending=false&id=%2Fsites%2FRInitative%2FShared%20Documents%2FProcess%5FDevelopment%2FB%2DS004%2EERxx%2BSTD%2BTeam%2BSteps%2Bfor%2BDeveloping%2Ba%2BInternal%2BR%2BPackage%2Edoc&parent=%2Fsites%2FRInitative%2FShared%20Documents%2FProcess%5FDevelopment"
)
# risk_col <- c(
#   "low" = "brightgreen",
#   "moderate" = "orange",
#   "open" = "red"
# )
#
# stage_col <- c(
#   "specification" = "orange",
#   "development" = "blue",
#   "validation" = "blue",
#   "customer review" = "blue",
#   "stable" = "brightgreen",
#   "deprecated" = "blue",
#   "retired" = "orange"
# )
#
# validation_col <- c(
#   "independent testing" = "blue",
#   "double programming" = "blue"
# )
#
# build_col <- c(
#   "failing" = "red",
#   "passing" = "brightgreen"
# )
#' @rdname badges

baamr_risk_badge <- function(risk) {
  risk <- match.arg(tolower(risk), badges[["risk"]])

  # colour <- risk_col[[risk]]
  href <- badges[["doclink"]]
  src <- paste0("![Risk: ", risk, "](inst/Rbadges/risk-", risk, ".svg)")
  paste0("[", src, "](", href, ")")
}
#' @rdname badges

baamr_sdlc_badge <- function(stage) {
  stage <- match.arg(tolower(stage), badges[["sdlc_stages"]])

  # colour <- stage_col[[stage]]
  href <- badges[["doclink"]]
  src <- paste0("![Lifecycle: ", stage, "](inst/Rbadges/lifecycle-", stage, ".svg)")
  paste0("[", src, "](", href, ")")
}
#' @rdname badges

baamr_validation_badge <- function(validation) {
  validation <- match.arg(tolower(validation), badges[["validation"]])

  # colour <- validation_col[[validation]]
  href <- badges[["doclink"]]
  src <- paste0("![Validation: ", validation, "](inst/Rbadges/validation-", validation, ".svg)")
  paste0("[", src, "](", href, ")")
}
#' @rdname badges

baamr_codecov_badge <- function(codecov = 0) {
  ## will use Jenkins link and create svg source file later!!!!
  if (codecov < 0 | codecov > 100) stop("the code coverage proportion should be bewtween 0 and 100")

  codecovcat <- ifelse(codecov < 20, "poor",
    ifelse(codecov < 60, "low",
      ifelse(codecov < 80, "medium", "high")
    )
  )
  href <- badges[["doclink"]]
  src <- paste0("![Code coverage: ", codecov, "](inst/Rbadges/coverage-", codecovcat, ".svg)")
  paste0("[", src, "](", href, ")")
}
#' @rdname badges

baamr_build_badge <- function(build = "failing") {
  ## will use Jenkins link and create svg source file later!!!!
  build <- match.arg(tolower(build), badges[["build"]])

  # colour <- build_col[[build]]
  href <- badges[["doclink"]]
  src <- paste0("![Build: ", build, "](inst/Rbadges/build-", build, ".svg)")
  paste0("[", src, "](", href, ")")
}
