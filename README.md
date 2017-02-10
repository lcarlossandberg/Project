#ReadMe
##Bad_Boids
This is a python 2 program, it creates a animation of boids interacting with set parameters.

###Installation
This package can be installed using pip with:<br /> 
`pip install git+git://github.com/lcarlossandberg/Bad_Boids`<br /> 
It can then be unistalled when finished using:<br /> 
`pip uninstall Boids`

###Usage
Once the package is installed it can be called using command:<br /> 
Boids<br />
This will run the default settings of:<br /> 
count: 50<br /> 
attraction: 0.01<br /> 
alert: 100<br /> 
formation: 10000<br /> 
strength: 0.125<br /> 

It can also be called with arguments:

Code | Meaning
---  | ---
`-h` or `--help` | displays the help for each call        
`--argfile *string*` | import the name of the yaml file containing the arguments
`--count *interger*` | takes an interger that is the number of boids
`--attraction *float*` | takes a float that is the strenght of the attraction between the boids
`--alert *float*` | takes a float that is the distance at which the boids try to avoid each other(alert distance) 
`--formation *float*` | takes a float that is the distance at which the boids try to match speeds 
`--strength *float*` | takes a float that is the strength with which the boids want to come together
`--save *bool*` | takes a bool of True or False as the wether you wish to save the animation, must have animation software to do this
`--name *string*` | takes a string which is the file name if you want to save, if this is not provided it will save as a file called 'boids_1.mp4'

Example calls of this are:<br />
`Boids --count 10 --attraction 0.1 --alert 10 --save True --name boids.mp4`<br /> 
`Boids --argfile data.yml`<br />

An example yml file can be found inside the fixtures file in testing along with the code used to generate it.
