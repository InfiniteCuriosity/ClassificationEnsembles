#' Carseats data
#'
#' This is the Carseats data as shown in the ISLR package.
#'
#' @format Carseats
#' A simulated data set with 400 observations and 11 rows
#' \describe{
#' \item{Sales}{Unit sales (in thousands) at each location}
#' \item{CompPrice}{Price charged by competitor at each location}
#' \item{Income}{Community income level (in thousands of dollars)}
#' \item{Advertising}{Local advertising budget for company at each location (in thousands of dollars)}
#' \item{Population}{Population size in region (in thousands)}
#' \item{Price}{Price company charges for car seats at each site}
#' \item{ShelveLoc}{A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site}
#' \item{Age}{Average age of the local population}
#' \item{Urban}{A factor with levels No and Yes to indicate whether the store is in an urban or rural location}
#' \item{US}{A factor with levels No and Yes to indicate whether the store is in the US or not}
#' }
#'
#' @source ISLR data set, https://www.rdocumentation.org/packages/ISLR/versions/1.4/topics/Carseats
"Carseats"



#' Dry Beans small
#'
#' This is a stratified version of the full dry beans data set. This is about 7 percent of the full data set
#'
#' @format dry_beans_small
#' A reduced version with 813 rows and 17 columns of the full data set available on UCI: https://archive.ics.uci.edu/dataset/602/dry+bean+dataset
#' \describe{
#'  \item{Area}{The area of a bean zone and the number of pixels within its boundaries}
#'  \item{Perimeter}{Bean circumference is defined as the length of its border}
#'  \item{MajorAxisLength}{The distance between the ends of the longest line that can be drawn from a bean}
#'  \item{MinorAxisLength}{The longest line that can be drawn from the bean while standing perpendicular to the main axis}
#'  \item{AspectRatio}{Defines the relationship between MajorAxisLength and MinorAxisLength}
#'  \item{Eccentricity}{Eccentricity of the ellipse having the same moments as the region}
#'  \item{ConvexArea}{Number of pixels in the smallest convex polygon that can contain the area of a bean seed}
#'  \item{EquivDiameter}{Equivalent diameter: The diameter of a circle having the same area as a bean seed area}
#'  \item{Extent}{The ratio of the pixels in the bounding box to the bean area}
#'  \item{Solidity}{Also known as convexity. The ratio of the pixels in the convex shell to those found in beans.}
#'  \item{Roundness}{Calculated with the following formula: (4piA)/(P^2)}
#'  \item{Compactness}{Measures the roundness of an object}
#'  \item{ShapeFactor1}{Continuous value}
#'  \item{ShapeFactor2}{Continuous value}
#'  \item{ShapeFactor3}{Continuous value}
#'  \item{ShapeFactor4}{Continuous value}
#'  \item{Class}{(Seker, Barbunya, Bombay, Cali, Dermosan, Horoz and Sira)}
#'  }
#'  @source https://archive.ics.uci.edu/dataset/602/dry+bean+dataset
"dry_beans_small"



#' Maternal Health Risk
#'
#' Data has been collected from different hospitals, community clinics, maternal health cares from the rural areas of Bangladesh through the IoT based risk monitoring system.
#'
#' @format Maternal_Health_Risk
#' Age, Systolic Blood Pressure as SystolicBP, Diastolic BP as DiastolicBP, Blood Sugar as BS, Body Temperature as BodyTemp, HeartRate and RiskLevel. All these are the responsible and significant risk factors for maternal mortality, that is one of the main concern of SDG of UN.
#'
#' \describe{
#'  \item{Age}{Any ages in years when a women during pregnant.}
#'  \item{SystolicBP}{Upper value of Blood Pressure in mmHg, another significant attribute during pregnancy.}
#'  \item{DiastolicBP}{Lower value of Blood Pressure in mmHg, another significant attribute during pregnancy.}
#'  \item{BS}{Blood glucose levels is in terms of a molar concentration}
#'  \item{BodyTemp}{Body temperature in Farenheit}
#'  \item{HeartRate}{A normal resting heart rate}
#'  \item{RiskLevel}{Predicted Risk Intensity Level during pregnancy considering the previous attribute.}
#' }
"Maternal_Health_Risk"

#' Cleveland Heart
#'
#' Posted by John Gennari, 3/13/90, This is Dr. Detrano's database modified to be a real MIXED dataset.
#'
#' @format Cleveland_heart
#' These are the original attributes:
#' Attributes: 8 symbolic, 6 numeric. Age; sex; chest pain type (angina, abnang, notang, asympt)
#' Trestbps (resting blood pres); cholesteral; fasting blood sugar < 120
#' (true or false); resting ecg (norm, abn, hyper); max heart rate;
#' exercise induced angina (true or false); oldpeak; slope (up, flat, down)
#' number of vessels colored (???); thal (norm, fixed, rever). Finally, the
#' class is either healthy (buff) or with heart-disease (sick).
#'
#' @source https://archive-beta.ics.uci.edu/dataset/45/heart+disease/files?path=cleve.mod
#'
#' The column names were corrected to be usable by R ('Chest_pain_type' instead of 'chest pain type'),
#' removed the '<' symbol from a column name because the '<' symbol causes some errors reading the column names (such as tree models),
#' removed three columns due to very high number of missing cells (noted as '?' in the original file.) Those three
#' columns are 'Number_of_vessels_colored', 'Thal', and 'Resting_ECG'.
#'
#' \describe{
#'  \item{Age}{Age of the subject}
#'  \item{Sex}{Sex of the subject, either male or female}
#'  \item{Chest_pain_type}{One of angina, abnang, notang, or asympt}
#'  \item{Resting_blood_pressure}{The resting blood pressure for the subject}
#'  \item{Cholesteral}{The patient's cholesterol}
#'  \item{Fasting_blood_sugar}{Binary, whether the fasting blood sugar is <120}
#'  \item{Max_heart_rate}{The maximum measured heart rate for the patient}
#'  \item{Exercise_induced_angina}{Binary, whether angina was induced due to exercise}
#'  \item{Old_peak}{Numeric value}
#'  \item{Slope}{Three levels: Down, flat, up}
#'  \item{Sick_or_buff}{Binary, is the patient sick or buff}
#'  \item{Class}{0 is healthy, 1,2,3,4 is sick.}
#' }
"Cleveland_heart"
