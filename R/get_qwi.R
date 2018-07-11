#' @title Get Quarterly Workforce Indicator Data
#' 
#' @name get_qwi
#' 
#' @description 
#' Queries the QWI using the Census API (note, this requires a Census API key)
#' 
#' @seealso \url{https://www.census.gov/data/developers/data-sets/qwi.html} for QWI API 
#' documentation from Census
#' 
#' @param worker_char Worker characteristic endpoint: "sex_age" or "sa"; 
#' "sex_educ" or "se";
#' "race_ethnicity" or "re"
#' 
#' @param indicator 
#'"Emp"
#'Beginning-of-Quarter Employment
#'Estimate of the total number of jobs on the first day of the reference quarter
#'
#'"EmpEnd"
#'End-of-Quarter Employment
#'Estimate of the number of jobs on the last day of the quarter
#'
#'"EmpS"
#'Full-Quarter Employment (Stable)
#'Estimate of stable jobs - the number of jobs that are held on both the first and 
#'last day of the quarter 
#'with the same employer
#'
#'"EmpTotal"
#'Employment - Reference Quarter
#'Estimated count of people employed in a firm at any time during the quarter
#'
#'"EmpSpv"
#'Full-Quarter Employment in the Previous Quarter
#'Estimate of stable jobs in the quarter before the reference quarter
#'
#'"HirA"
#'Hires (All Accessions)
#'Estimated number of workers who started a new job in the specified quarter
#'
#'"HirN"
#'New Hires
#'Estimated number of workers who started a new job excluding recall hires
#'
#'"HirR"
#'Recall Hires
#'Estimated number of workers who returned to the same employer where they had worked 
#'within the previous year
#'
#'"Sep"
#'Separations (All)
#'Estimated number of workers whose job with a given employer ended in the 
#'specified quarter
#'
#'"HirAEnd"
#'End-of-Quarter Hires
#'Estimated number of workers who started a new job in the specified quarter, 
#'which continued into next quarter
#'
#'"SepBeg"
#'Beginning-of-Quarter Separations
#'Estimated number of workers whose job in the previous quarter continued and 
#'ended in the given quarter
#'
#'"HirAEndRepl"
#'Replacement Hires
#'Hires into continuous quarter employment in excess of job creation
#'
#'"HirAEndR"
#'End-of-Quarter Hiring Rate
#'Hires as a percent of average employment
#'
#'"SepBegR"
#'Beginning-of-Quarter Separation Rate
#'Separations as a percent of average employment
#'
#'"HirAEndReplR"
#'Replacement Hiring Rate
#'Replacement hires as a percent of the average of beginning- and end-of-quarter employment
#'
#'"HirAS"
#'Hires (All Hires into Full-Quarter Employment)
#'Estimated number of workers that started a job that lasted at least one full quarter 
#'with a given employer
#'
#'"HirNS"
#'New Hires (New Hires into Full-Quarter Employment)
#'Estimated number of workers who started a job that they had not held within the past year and 
#'the job turned into a job that lasted at least a full quarter with a given employer
#'
#'"SepS"
#'Separations (Flows out of Full-Quarter Employment)
#'Estimated number of workers who had a job for at least a full quarter and then the job ended
#'
#'"SepSnx"
#'Separations in the Next Quarter (Flows out of Full-Quarter Employment)
#'Estimated number of workers in the next quarter who had a job for at least a full quarter and 
#'then the job ended
#'
#'"TurnOvrS"
#'Turnover (Stable)
#'The rate at which stable jobs begin and end
#'
#'"FrmJbGn"
#'Firm Job Gains (Job Creation)
#'Estimated number of jobs gained at firms throughout the quarter
#'
#'"FrmJbLs"
#'Firm Job Loss (Job Destruction)
#'Estimated number of jobs lost at firms throughout the quarter
#'
#'"FrmJbC"
#'Firm Job Change (Net Change)
#'Difference between firm job gain and firm job loss
#'
#'"FrmJbGnS"
#'Firm Job Gains (Stable)
#'Estimated number of full-quarter jobs gained at firms
#'
#'"FrmJbLsS"
#'Firm Job Loss (Stable)
#'Estimated number of full-quarter jobs lost at firms
#'
#'"FrmJbCS"
#'Firm Job Change (Stable; Net Change)
#'Net growth in jobs that last a full quarter
#'
#'"EarnS"
#'Average Monthly Earnings (Full-Quarter Employment)
#'Average monthly earnings of employees with stable jobs
#'
#'"EarnBeg"
#'Average Monthly Earnings (Beginning-of-Quarter Employment)
#'Average monthly earnings of employees who worked on the first day of the reference quarter
#'
#'"EarnHirAS"
#'Average Monthly Earnings (All Hires into Full-Quarter Employment)
#'Average monthly earnings for workers who started a job that turned into a job lasting a full quarter
#'
#'"EarnHirNS"
#'Average Monthly Earnings (New Hires into Full-Quarter Employment)
#'Average monthly earnings of newly stable employees
#'
#'"EarnSepS"
#'Average Monthly Earnings (Flows out of Full-Quarter Employment)
#'Average monthly earnings of separations from full-quarter status at an establishment
#'
#'"Payroll"
#'Total Quarterly Payroll
#'Total quarterly payroll for all jobs
#'
#'@param key Census API key
#' 
#'


#adds census API key

if (!is.null(key)) {
  
  Sys.setenv(CENSUS_API_KEY = key)
}


if (Sys.getenv('CENSUS_API_KEY') != '') {
  
  key <- Sys.getenv('CENSUS_API_KEY')
  
} else if (is.null(key)) {
  
  stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, 
       and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')
  
}