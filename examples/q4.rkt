{
    list_of string input = (list (("020250112") ("20190418") ("20230928") 
                        ("20241515")("20210101")("20001231")("20250618")));



    num func char2num(string chr)
    {
        if ((chr) == ("0")) then { (0); }else{   
        if ((chr) == ("1")) then { (1); }else{
        if ((chr) == ("2")) then { (2); }else{
        if ((chr) == ("3")) then { (3); }else{
        if ((chr) == ("4")) then { (4); }else{  
        if ((chr) == ("5")) then { (5); }else{
        if ((chr) == ("6")) then { (6); }else{
        if ((chr) == ("7")) then { (7); }else{
        if ((chr) == ("8")) then { (8); }else{
        if ((chr) == ("9")) then { (9); 
        }else{
            (0);
            };
            };
            };
            };
            };
            };
            };
            };
            };
            };
    };

    num func string2num(string str)
    {
        num len = (string-length(str));
        num at = (0);
        num result = (0);
        while ((at) < (len)) 
        {   
            string digit_str = (string-at (str) (at));
            num digit_num = (char2num((digit_str)));
            result = (((result) * (10)) + (digit_num));
            
            at = ((at) + (1));
        };
        (result);
    };

    list_of string  func filter_len(list_of string ls) 
    {
        list_of string filtered = (list ()); /* empty list[string] */ 
        
        while (not (is-empty (ls))) {
            string str = (car (ls));
            if ((string-length (str)) == (8)) then {
                filtered = (append (filtered) (list ((str))));
            }else {filtered;};

            ls = (cdr (ls));
        };
        filtered;
    };

    list_of num func list_string2list_num(list_of string ls)
    {
        list_of num result = (list ());
        while (not (is-empty (ls))) {
            string s = (car (ls));
            num n = (string2num((s)));
            result = (cons (n) (result));

            ls = (cdr (ls));
        };
        result;
    };



    list_of string filtered = (filter_len((input)));
    list_of num numed = (list_string2list_num((filtered)));

    numed = (numed[0] = (9));
    
    numed;



    /* string2num(("0123456789")); */
}