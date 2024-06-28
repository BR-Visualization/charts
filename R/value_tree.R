#' Create Value Tree
#'
#' Generate example value tree
#' @inheritParams DiagrammeR::mermaid
#'
#' @return value tree image
#' @export
#'
#' @examples
#' value_tree(
#' diagram =
#' "graph LR;
#' A(<B>Benefit-Risk Balance</B>)-->B(<B>Benefits</B>)
#' B-->C(<B>Primary Efficacy</B>)
#' B-->D(<B>Secondary Efficacy</B>)
#' B-->E(<B>Quality of life</B>)
#' C-->F(<B>% Success</B>)
#' D-->G(<B>Mean change</B>)
#' E-->H(<B>Mean change</B>)
#' A-->I(<B>Risks</B>)
#' I-->J(<B>Recurring AE</B>)
#' I-->K(<B>Rare SAE</B>)
#' I-->L(<B>Liver Toxicity</B>)
#' J-->M(<B>Event rate</B>)
#' K-->N(<B>% Event</B>)
#' L-->O(<B>% Event</B>)
#' style A fill:#7ABD7E
#' style B fill:#7ABD7E
#' style I fill:#7ABD7E
#' style C fill:#FFE733
#' style D fill:#FFE733
#' style E fill:#FFE733
#' style J fill:#FFE733
#' style K fill:#FFE733
#' style L fill:#C6C6C6
#' style F fill: #FFAA1C
#' style G fill: #FFAA1C
#' style H fill: #FFAA1C
#' style M fill: #FFAA1C
#' style N fill: #FFAA1C
#' style O fill: #C6C6C6
#' ")
value_tree <- function(diagram, ...) {
  DiagrammeR::mermaid(diagram = diagram, ...)
}
