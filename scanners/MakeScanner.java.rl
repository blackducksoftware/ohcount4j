package net.ohloh.ohcount4j.scan;

public class MakeScanner extends BaseScanner{
  int ab_count = 0;
  int cd_count = 0;
  %%{
    machine ex;
    action found_ab { ab_count++; }
    action found_cd { cd_count++; }
    x = "ab" %found_ab;
    y = "cd" %found_cd;
    main := "_" (x|y)* "_";
  }%%

  %% write data;

  @Override
  public void doScan(){
		// variables and data is set up in BaseScanner
    %% write init;
    init();
    %% write exec;
  }

}
