# Final Project for Phylo-Class 563
(Last updated 2021-04-09)


## Project summary
In this project I simulate genetic data subject to intralocus recombination under various parameter regimes (mutation rate, species tree branch lengths, recombination rates, etc) in order evaluate the robustness of summary coalescent methods to violations of the "no intralocus recombination" assumption. 

For the draft overview of my project and reproducible script, see [notebook-log.md](notebook-log.md).

For further information on the scripts used, see the [scripts readme](scripts/readme.md).
## To do
* Finish implementing mutations. To do this you should think clearly about what
  you want your pipeline to look like. What outputs should be saved as files?
* Implement *BEAST
* Combine the 'execute-' script files. Maybe do this instead of writing an
  additional "execute-" file for jc-sequence.
* Add functionality for 4-letter sequences ein simulate.sh
* Write something reproducible for makeing a plot from simulated data. 
* Expand plotmaking functionality and smooth out filename wrinkles.
* Improve documentation. Remove outdated comments in script files. 
