`%==%` = function(x1,x2){
    identical(x1,x2)
}

`%!==%` = function(x1,x2){
    !identical(x1,x2)
}

check_windows_strings = function(strings){
    .Platform$OS.type %==% "windows" &&
        all(Encoding(strings) == "UTF-8")
}

update_windows_strings = function(){
    .Platform$OS.type %==% "windows"
}