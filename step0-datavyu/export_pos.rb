require 'Datavyu_API.rb'

begin
    #id = getColumn("id")
    
    #Create the output file
    out_file = File.expand_path("~/Desktop/activity.csv")
	out = File.new(out_file,'w')
    
    #Get the data from datavyu
    trial = getColumn("position")
    id = getColumn("sync")

    for tcell in trial.cells
        out.write((tcell.onset - id.cells[0].onset).to_s + "," + (tcell.offset - id.cells[0].onset).to_s + "," + tcell.pos)
        # out.write("\n")
    end
    # out.write((id.cells[1].onset - id.cells[0].onset).to_s + "," + (id.cells[1].offset - id.cells[0].onset).to_s + ",activity\n")
    # out.write((id.cells[2].onset - id.cells[0].onset).to_s + "," + (id.cells[2].offset - id.cells[0].onset).to_s + ",sync\n")
    # out.write((id.cells[3].onset - id.cells[0].onset).to_s + "," + (id.cells[3].offset - id.cells[0].onset).to_s + ",freeplay")
end
