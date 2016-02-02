rm('C:/Users/TSR/Desktop/log/[KTXP][LOG_HORIZON.Ⅱ][01][GB][X264_AAC][720p][CHS]/[Dymy][LOG_HORIZON.II][01][BIG5][1280X720][CHT].MP4.part1.exe')
  
for (i in 1:23){
  a1=sprintf('C:/Users/TSR/Desktop/log/[KTXP][LOG_HORIZON.Ⅱ][01][GB][X264_AAC][720p][CHS]/[Dymy][LOG_HORIZON.II][%02d][BIG5][1280X720][CHT].MP4.part1.exe',i)
  
  b1=sprintf('C:/Users/TSR/Desktop/log/[KTXP][LOG_HORIZON.Ⅱ][01][GB][X264_AAC][720p][CHS]/[Dymy][LOG_HORIZON.II][%02d][BIG5][1280X720][CHT].MP4.part2.rar',i)
  
  file.remove(a1,b1)
}