require 'Datavyu_API.rb'

begin
    #id = getColumn("id")
    
    #Create the output file
    out_file = File.expand_path("~/Desktop/activity_pt2.csv")
	out = File.new(out_file,'w')
    
    #Get the data from datavyu
    trial = getColumn("position")

    for tcell in trial.cells
        out.write((tcell.onset).to_s + "," + (tcell.offset).to_s + "," + tcell.pos)
        out.write("\n")
    end
end
