{
    num SIZE = (6);

    bool func in_range(num idx)
    {
        if (((idx) < (SIZE)) and ((idx) >= (0))) then
        {
            (true);
        }
        else
        {
            (false);
        };
    };

    bool func is_a_match (string player, list_of list_of string board, num row, num col, num row_inc, num col_inc)
    {
        if ((in_range(((row) + ((row_inc) * (3))))) and (in_range(((col) + ((col_inc) * (3)))))) then
        {
            num count = (3);
            bool matched = (true);
            while ((count) >= (0))
            {
                if ((board[row][col]) != (player)) then
                {
                    count = (-(1));
                    matched = (false);
                }
                else
                {
                    row = ((row) + (row_inc));
                    col = ((col) + (col_inc));
                    count = ((count) - (1));
                    (matched);
                };
            };
            (matched);
        }
        else
        {
            (false);
        };
    };
    list_of list_of string board = (list((list(("-") ("X") ("-") ("-") ("X") ("O")))
                                         (list(("O") ("O") ("X") ("-") ("-") ("O")))
                                         (list(("-") ("-") ("X") ("O") ("-") ("O")))
                                         (list(("-") ("X") ("X") ("X") ("X") ("O")))
                                         (list(("O") ("O") ("-") ("-") ("X") ("-")))
                                         (list(("-") ("-") ("-") ("-") ("-") ("X")))));

    list_of num row_dirs = (list((0) (1) (1)));
    list_of num col_dirs = (list((1) (0) (1)));

    num row = (0);
    num col = (0);
    num dir_idx = (0);
    bool x_won = (false);
    bool o_won = (false);
    while ((row) < (SIZE))
    {
        col = (0);
        while ((col) < (SIZE))
        {
            dir_idx = (0);
            while ((dir_idx) < (3))
            {
                if (is_a_match (("X") (board) (row) (col) (row_dirs[dir_idx]) (col_dirs[dir_idx]))) then
                {
                    x_won = (true);
                }
                else
                {
                    (x_won);
                };

                if (is_a_match (("O") (board) (row) (col) (row_dirs[dir_idx]) (col_dirs[dir_idx]))) then
                {
                    o_won = (true);
                }
                else
                {
                    (o_won);
                };
                dir_idx = ((dir_idx) + (1));
            };
            col = ((col) + (1));
        };
        row = ((row) + (1));
    };

    if (x_won) then
    {
        if (o_won) then
        {
            /=\ ("Somebody Cheated!");
        }
        else
        {
            /=\ ("X wins!");
        };
    }
    else
    {
        if (o_won) then
        {
            /=\ ("O Wins!");
        }
        else
        {
            /=\ ("Draw!");
        };
    };

}