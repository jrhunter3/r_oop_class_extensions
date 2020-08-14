##### Create the base Agent class #####
# # This is used to represent the most basic agent in a simulation

Agent <- setClass(
  # Set the name for the class
  "Agent",
  
  #Define the slots
  slots = c(
    location = "numeric",
    idle = "logical",
    stationary = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    location = c(0, 0, 0),
    idle = TRUE,
    stationary = TRUE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(!is.logical(object@stationary)){
      return("Parameter stationary is an invalid class")
    }
    return(TRUE)
  }
)

# create a method to get the value of the location
setGeneric(name="getLocation",
           def=function(theObject)
           {
             standardGeneric("getLocation")
           }
)
setMethod(f="getLocation",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@location)
          }
)

# create a method to assign the value of location
setGeneric(name="setLocation",
           def=function(theObject, position)
           {
             standardGeneric("setLocation")
           }
)
setMethod(f="setLocation",
          signature="Agent",
          definition=function(theObject, position)
          {
            theObject@location <- position
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of stationary
setGeneric(name="getStationary",
           def=function(theObject)
           {
             standardGeneric("getStationary")
           }
)
setMethod(f="getStationary",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@stationary)
          }
)

# create a method to assign the value of stationary
setGeneric(name="setStationary",
           def=function(theObject, stationary)
           {
             standardGeneric("setStationary")
           }
)
setMethod(f="setStationary",
          signature="Agent",
          definition=function(theObject, stationary)
          {
            theObject@stationary <- stationary
            validObject(theObject)
            return(theObject)
          }
)

##### Create the Machine class #####
#
# This is used to represent a machine
Person <- setClass(
  # Set the name for the class
  "Machine",
  
  #Define the slots
  slots = c(
    switchedOn = "logical",
    needsMaintenance = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    switchedOn = FALSE,
    needsMaintenance = FALSE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(!is.logical(object@stationary)){
      return("Parameter stationary is an invalid class")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains="Agent"
)

##### Create the SewingMachine class #####
#
# This is used to represent a sewing machine
SewingMachine <- setClass(
  # Set the name for the class
  "SewingMachine",
  
  #Define the slots
  slots = c(
    hasNeedle = "logical",
    hasBobbin = "logical",
    hasThread = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    hasNeedle = TRUE,
    hasBobbin = TRUE,
    hasThread = TRUE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(!is.logical(object@stationary)){
      return("Parameter stationary is an invalid class")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains="Machine"
)

##### Create the Person class #####
#
# This is used to represent a person
Person <- setClass(
  # Set the name for the class
  "Person",
  
  #Define the slots
  slots = c(
    velocity = "numeric",
    acceleration = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    velocity = c(0, 0 , 0),
    acceleration = c(0, 0 , 0)
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100){
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains="Agent"
)

# create a method to get the value of the velocity
setGeneric(name="getVelocity",
           def=function(theObject)
           {
             standardGeneric("getVelocity")
           }
)
setMethod(f="getVelocity",
          signature="Person",
          definition=function(theObject)
          {
            return(theObject@velocity)
          }
)

# create a method to assign the value of velocity
setGeneric(name="setVelocity",
           def=function(theObject, velocity)
           {
             standardGeneric("setVelocity")
           }
)
setMethod(f="setVelocity",
          signature="Person",
          definition=function(theObject, velocity)
          {
            theObject@velocity <- velocity
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of acceleration
setGeneric(name="getAcceleration",
           def=function(theObject)
           {
             standardGeneric("getAcceleration")
           }
)
setMethod(f="getAcceleration",
          signature="Person",
          definition=function(theObject)
          {
            return(theObject@acceleration)
          }
)

# create a method to assign the value of acceleration
setGeneric(name="setAcceleration",
           def=function(theObject, acceleration)
           {
             standardGeneric("setAcceleration")
           }
)
setMethod(f="setAcceleration",
          signature="Person",
          definition=function(theObject, acceleration)
          {
            theObject@acceleration <- acceleration
            validObject(theObject)
            return(theObject)
          }
)

##### Create the Worker class #####
#
# This is used to represent a worker
Worker <- setClass(
  # Set the name for the class
  "Worker",
  
  #Define the slots
  slots = c(
    onBreak = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    onBreak = FALSE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100){
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains="Person"
)