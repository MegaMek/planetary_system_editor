read_planetary_system <- function(file) {
  
  system_xml <- xml2::read_xml("example_system.xml")
  
  id <- xml2::xml_text(xml_find_first(system_xml, "id"))
  sucsId <- as.numeric(xml2::xml_text(xml_find_first(system_xml, "sucsId")))
  x <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "xcood")))
  y <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "ycood")))
  spectralType <- xml2::xml_text(xml2::xml_find_first(system_xml, "spectralType"))
  primarySlot <- as.numeric(xml2::xml_text(xml2::xml_find_first(system_xml, "primarySlot")))
  
  system_events <- purrr::map(xml2::xml_find_all(system_xml, "event"),
                              read_event)
  
  system_planets <- purrr::map(xml2::xml_find_all(system_xml, "planet"),
                               read_planet)
  
  list(id = id,
       sucsId = sucsId,
       x = x,
       y = y,
       spectralType = spectralType,
       primarySlot = primarySlot,
       planet = system_planets)
  
}

read_planet <- function(planet_xml) {
  
  name <- xml2::xml_text(xml_find_first(planet_xml, "name"))
  type <- xml2::xml_text(xml_find_first(planet_xml, "type"))
  orbitalDist <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "orbitalDist")))
  sysPos <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "sysPos")))
  pressure <- xml2::xml_text(xml_find_first(planet_xml, "pressure"))
  atmosphere <- xml2::xml_text(xml_find_first(planet_xml, "atmosphere"))
  composition <- xml2::xml_text(xml_find_first(planet_xml, "composition"))
  gravity <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "gravity")))
  dayLength <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "dayLength")))
  diameter <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "diameter")))
  density <- as.numeric(xml2::xml_text(xml_find_first(planet_xml, "density")))
  
  planet_events <- purrr::map(xml2::xml_find_all(planet_xml, "event"),
                              read_event)
  
  planet <- list(name = name,
                 type = type,
                 orbital_dist = orbitalDist,
                 sysPos = sysPos,
                 pressure = pressure,
                 atmosphere = atmosphere,
                 composition = composition,
                 gravity = gravity,
                 dayLength = dayLength,
                 diameter = diameter,
                 density = density,
                 event = planet_events)
  
}

read_event <- function(events_xml) {
  
  # get values
  values <- xml2::xml_text(xml_children(events_xml))
  # name the values
  names(values) <- xml2::xml_name(xml_children(events_xml))
  # coerce to a list
  values <- as.list(values)
  
  # convert numbers back to numeric values
  values <- map(values, function(x) {
    ifelse(stringr::str_detect(x, "\\D"), x, as.numeric(x))
  })
  
  return(values)
  
}

# you can use map on xml_nodesets
#purrr::map(system_planets, function(x) {xml2::xml_text(xml2::xml_find_first(x, "name"))})

# you can grab attributes like this.
#xml2::xml_attr(xml2::xml_find_first(system_xml, "spectralType"), "source")

cat(yaml::as.yaml(read_planetary_system("test"), indent.mapping.sequence = TRUE, precision = 6))
