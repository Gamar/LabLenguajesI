#
# Clase que contiene las maquinas por las que esta compuesto
# el sistema
#
class Maquina
	attr_reader :nombre, :estado, :insumos, :cant_max, :porcentaje, :desecho, :ciclos  

	def initialize()
		@nombre = ""
	    @insumos = ""
	    @estado = "inactiva"
	    @cant_max = 0 
	    @porcentaje = 100
	    @desecho = 0
	    @ciclos = 0
	end

	#abstract procesar()

	#abstract imprimir()

end

class Silos_Cevada < Maquina
  	def initialize()  
    	super
    	@nombre = "Silos de Cebada"
    	@insumos = "C"
    	@cant_max = 400 
  	end

  	def imprimir()
  		print "Maquina: ",@nombre,"\n"
  		print "Estado: ",@estado,"\n"
  	end


end

 
b = Silos_Cevada.new
puts b.insumos  
puts b.cant_max  
puts b.porcentaje
puts b.desecho
puts b.ciclos
b.imprimir

