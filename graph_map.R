graph_map<-function(var_graph,party_color,varname_title,data_name, title_graph){
  
  if(party_color %in% c("MORENA","MORENA_C")){
    ggplot() +
      geom_sf(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
      scale_fill_gradient(low="white",high="maroon4")+
      ggtitle(title_graph)+
      geom_text(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
       labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
    
#    ggplot() +
 #     geom_sf(data = list_master[[elecc_num]][list_master[[elecc_num]]$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
  #    scale_fill_gradient(low="white",high="maroon4")+
   #   ggtitle(paste("Distrito 4. ",election_type," ",year_state))+
    #  geom_text(data = list_master[[elecc_num]][list_master[[elecc_num]]$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
     # labs(fill=varname_title)+
      #theme_void() 
    
#    if(party_color %in% c("MORENA","MORENA_C")){
 #     ggplot() +
  #      geom_sf(data = master[master$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
   #     scale_fill_gradient(low="white",high="maroon4")+
    #    ggtitle(paste("Distrito 4. ",election_type," ",year_state))+
     #   geom_text(data = master[master$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
      #  labs(fill=varname_title)+
       # theme_void()     
    
  } else if (party_color %in% c("PAN_C","PAN")){
    ggplot() +
      geom_sf(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
      scale_fill_gradient(low="white",high="dodgerblue4")+
      ggtitle(title_graph)+
      geom_text(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
      labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
    
  } else if (party_color=="DIFF"){
    ggplot() +
      geom_sf(data = data_name[data_name$SECCION %in% list_sections,], aes(fill=x), show.legend = T, color="black") +
      scale_fill_gradient2(midpoint=0, low="lightpink3", mid="white", high="dodgerblue4", space="Lab")+
      geom_text(data = master[master$SECCION %in% list_sections,], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
      labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
    
  } else if (party_color %in% c("MC","PRD","PRD_MC")) {
    ggplot() +
      geom_sf(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
      scale_fill_gradient(low="white",high="orange2")+
      ggtitle(title_graph)+
      geom_text(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
      labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
    
  } else if (party_color %in% c("PRI","PRI_PVEM")) {
    ggplot() +
      geom_sf(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
      #scale_fill_gradient(low="palegreen",high="palegreen4")+
      scale_fill_gradient(low="white",high="palegreen4")+
      ggtitle(title_graph)+
      geom_text(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
      labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
  
  } else if (party_color %in% c("LISTA_NOMINAL","PARTICIPACION")) {
  ggplot() +
    geom_sf(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes_string(fill=var_graph), show.legend = T, color="black") +
    scale_fill_gradient(low="white",high="violetred4")+
    ggtitle(title_graph)+
    geom_text(data = data_name[data_name$SECCION %in% list_sections[[dist_local]],], aes(X, Y, label = SECCION), size = 3.3, color='gray0', fontface="bold") +
    labs(fill=varname_title)+
      theme_void()+
      theme(plot.title=element_text(size=20),plot.subtitle=element_text(size=15)) 
  
}
}

#plots=lapply(distrito_id_num, graph_map,party)


#plots=lapply(distrito_id_num, graph_map,party)
#names(plots)=distrito_id_num
#lapply(names(plots), function(x) ggsave(filename=paste(state_name,year,"Dist",x,party,".jpeg",sep="_"), plot=plots[[x]]))
