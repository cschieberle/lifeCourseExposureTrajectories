<p align="right">
<img src="http://www.heals-eu.eu/wp-content/uploads/2013/10/logo_heals_spacing.png">
</p>

# lifeCourseExposureTrajectories
R package for life-long exposure assessment towards the exposome.

Please refer to the following source when using the library:

> **Schieberle, C.; Li, N.; Friedrich, R.: *Life course exposure trajectories: A probabilistic model for prospective and retrospective exposure characterization.* 19th International Symposium on Environmental Pollution and its Impact on Life in the Mediterranean, Rome, Italy, 4-6 October 2017.**

## Aims and purpose

By assessing determinants of life trajectories we aim to estimate past exposure retrospectively and future exposure prospectively as well for individuals as for vulnerable groups. The model will be defined in a probabilistic manner to allow for incorporation of uncertainty during all stages of the assessment. A prototype implementation of the concept is presented in this section.

In social sciences, sequence analysis is a key approach to study life trajectories (Studer & Ritschard (2016), Widmer & Ritschard (2009)). It allows to identify trajectory patterns that account for all states of interest and, more importantly, transitions between states. For the first time we will use this analysis approach in the field of exposure science to develop a probabilistic life exposure trajectory model that links life trajectories with time-activity patterns and an activity-exposure matrix to establish a life exposure trajectory model. 

In this section we present the modelling approach towards prospective and retrospective exposure characterization using estimates of life course trajectories. The goal is to use information collected at individual or group level to reconstruct past and estimate future life trajectories. We developed this approach as part of a 3-level methodology in which
-	Level 1: Estimate life trajectories of main activities (years in a lifetime)
-	Level 2: Estimate time-activity within the year (minutes in a day)
-	Level 3: Estimate exposure at location/activity level

For the Level 1 part of the methodology, we use data from longitudinal studies in which surveys (here: 4-year) were conducted (see HEALS D11.1 for a description). Time-activity patterns (i.e. episode files of individuals) as the one described in HEALS D11.1) are used in the Level 2 part of the methodology. The exposure estimation follows along the approach sketched in D11.1 utilizing the environmental data and exposure data from Stream 3 data synthesis efforts.

However, this package focuses on the first level of this overall approach, i.e. the life trajectories. Based on an individual’s current stage in life and several life-course characterizing fac-tors like age, gender and current educational and/or current employment situation we estimate potential future life-course trajectories along with their probability of occurrence. For groups of individ-uals, we estimate the potential future life trajectories in a very similar way and, if unknown, estimate probabilities of past trajectories that may have happened.

The backbone of the estimation approach is to analyze collections of recorded life trajectories of individuals retrospectively and regress certain socio-economic factors with the aim of transferring the information and predict future developments in life and estimate past trajectories in case they are not known.

## Descriptive and explanatory analysis of longitudinal survey data

The surveys use a specific design. Individuals are followed for a period of 4 years, that is in the interview year 2010, for example, we might have interviewees belonging to up to 4 surveys in which they might be interviewed the first, second, third or fourth time. We use the data to construct 4-year trajectories.
Household data is collected along with the individual, i.e. we have information on gross or disposa-ble income, dwelling type, etc. (similar to the data collected in the interviews in the pilot study), along with the individual data like age and sex, current economic situation (self-defined), highest level of education attained (and when or whether currently in education), or the marital status.
The following sections make use of the functionalities provided by the TraMineR package for the R programming language. See here for details:
> **Gabadinho, A., Ritschard, G., Müller, N.S. & Studer, M. (2011), Analyzing and visualizing state sequences in R with TraMineR, Journal of Statistical Software. Vol. 40(4), pp. 1-37.**

## Modelling approach

### General approach description

In summary, the basic algorithm of the simulation process works as follows. Note, that this is only the first level of the above mentioned 3-level methodology:
-	Start at the current stage in life of the individual. For simplicity we focus here on age and sex only, where sex remains constant, but is an important determinant of the life-course, and age will increase in 1-year steps.
-	In each step, the individual ‘enters’ the regression tree and a general life-stage is assessed. This corresponds to one of the leaf nodes of the tree. An example is shown in the figure below.
![Regression tree](https://raw.githubusercontent.com/cschieberle/lifeCourseExposureTrajectories/master/README-img/regression-tree.png)
-	The current situation determines the next (prospective) or the past years (retrospective). These are selected from the sequences that are grouped within each of the leaf nodes of the regres-sion tree. Therefore, the individual’s life course is probabilistically estimated using a Markov Chain that is constructed from the inner leaf-node states. An example is shown in the figure below this text.
![Markov chain](https://raw.githubusercontent.com/cschieberle/lifeCourseExposureTrajectories/master/README-img/markov-chain.png)
-	In a last step the age of the individual is adjusted (i.e. increased for prospective estimates and decreased for retrospective analysis). Once an age below 16 is reached, a separate model is used for the assessment.

Details of the approach as well as a discussion of the limitations of the approach and data availability are given in HEALS D9.2B.
This includes the description of the multi-factor regression analysis to form a tree of life-course trajectories as well as the construction of a Markov chain of inner-leaf node state transtitions.
Furtheremore, a model of childhood, teen-ageing and adolescence is introduced in the document.

### 
