require 'Datavyu_API.rb'

begin

    #Get the data from datavyu
    pos = getColumn("position")

    ncol = new_column("rel_blocks","block")
    ncell = ncol.new_cell
    ncell.onset = pos.cells[0].onset
    ncell.offset = ncell.onset + 30*60*1000

    set_column(ncol)
end
