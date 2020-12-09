function validate_required(field, alerttxt){
    with(field){
        if(value==null || value==""){
            alert(alerttxt);
            return false;
        } else {
            return true;
        }
    }
}

        
        
