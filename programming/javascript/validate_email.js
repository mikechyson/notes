function validate_email(field, alerttxt){
    with(field){
        apos=value.indexOf("@");
        dotpos=value.indexOf(".");
        if (apos<1 || dotpos - apos <2){
            alert(alerttxt);
            return false;
        } else {
            return true;
        }
    }
}
