# jsm-flight-2025

*Please check the `issues` tab on GitHub for your TODO list*

Speed: A hybrid of a poster and oral presentation, a speed session consists of 20 oral presentations of four minutes each, with five minutes of break after the first set of 10 talks.
The short oral presentations are then followed by an e-poster (offered on a 42” LCD display in the landscape position) session later the same day.

[Poster guidelines](https://ww2.amstat.org/meetings/jsm/2025/postertips.cfm#speed)

The repo contains:

-   `index.qmd`: the final poster/slides document
-   `scripts/`: analysis code scripts
-   `data-raw/`: the scripts to generate the saved data in the `data/` folder
-   `data/`: the data used in the article
-   `figures/`: saved figures

## Prompt

Participants in the 2025 Data Expo Challenge will develop a research question to explore, analyze, and visualize flight arrival and departure data for all commercial flights across the USA.
Uncover new insights in the skies as we revisit the 2009 Data Expo, a past favorite.
With 16 years more data, this rich dataset offers endless possibilities for creative exploration.This is your opportunity to uncover insights and tell a compelling story through data visualization and innovative analysis.
Participants must use the flight data and at least one additional dataset.

Preliminary Questions (from prompt): To get started here are some of the potential questions from the 2009 challenge: - When is the best time of day/day of week/time of year to fly to minimize delays?
- Do older planes suffer more delays?
- How does the number of people flying between different locations change over time?
- How well does weather predict plane delays?
- Can you detect cascading failures as delays in one airport create delays in others?
Are there critical links in the system?
If so, what are they and how do we find them?

As well as few new ones we brainstormed this year: - Can you predict the probability that your flight to the JSM is delayed?
- How long did it take for flights to recover from the pandemic?
Where there any structural changes in flight routes?
- Can you detect changes in estimated flight times to see if airlines are reducing the appearance of delays by adding some padding to flight times?

Data: - [Link to dataset](https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr) - [Link to data dictionary](https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ) - [A potential passenger data](https://www.transtats.bts.gov/DatabaseInfo.asp?QO_VQ=EED&QO_fu146_anzr=Nv4%20Pn44vr45&QO_anzr=Nv4%20Pn44vr4%20f6n6v56vp5%20(S14z%20HE%20g4nssvp)-%20%20h.f.%20Pn44vr45&Yv0x=D)

## Proposed abstract

**title**: The Effect of Delays on Airport Flight Patterns

**abstract**: Flight delays can be caused by events such as hazardous weather, crew availability and security issues. When purchasing flight tickets, many passengers hope to minimize delays to avoid spending too much time at the airport or missing a connecting flight. Flight delays often have a cascading effect, where one flight's delay may influence the next. Additionally, multiple flights may experience delays at the same time, when events such as bad weather occur. We hypothesize that airline "hubs" - defined here as an airport/airline pair containing a large volume of passenger traffic for that airline - may be more equipped to respond to delay perturbations than non-hubs. Herein, a Fast Fourier Transform (FFT) is applied to scheduled arrival/departure times to estimate airport periodicity. The relationship between hub status, periodicity, and delays is explored. We also compare differences between traditional "hub and spoke" airlines such as Delta to "point to point" structured airlines such as Southwest.

## Submitted abstract

~~Climate change and extreme weather events continue to plague the United States economy, including flight arrival times. In this poster, we plan to examine how extreme weather events affect airplane traffic. Specifically, we associate extreme weather events with predicted flight delays and build a model to predict the average expected delay given a specified weather forecast and season.~~

**title**: The Effect of Sports Teams Rankings On Recreational Air Travel

**abstract**: Dedicated sports fans may choose to fly to support their favorite team in person.
In this exploratory analysis, we investigate the relationship between seasonal rankings of sports teams and airplane traffic to home and away cities hosting collegiate and professional football games.
We uncover unique insights regarding the seasonal and behavioral traffic patterns of sports fans, with the Super Bowl as a case study.
