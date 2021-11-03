require 'Datavyu_API.rb'

begin
    #Create the output file
    out_file = File.expand_path("~/Desktop/pos_disagreements.csv")
	out = File.new(out_file,'w')
    
    #Get the data from datavyu
    trial = getColumn("pos_disagree")

    for tcell in trial.cells
        out.write(tcell.ordinal.to_s + ","+ tcell.onset.to_s + "," + tcell.offset.to_s + "," + tcell.pri_pos + "," + tcell.rel_pos + "," + (tcell.offset - tcell.onset).to_s)
        out.write("\n")
    end
end
